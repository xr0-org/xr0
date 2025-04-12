#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "state.h"
#include "util.h"
#include "verifier.h"

#include "path.h"

struct path {
	enum phase { EXEC, INV, ATEND } phase;

	struct state *state;

	struct state *inv;
	char *label; /* label of invariant under consideration */
};

static struct path *
_create(enum phase phase)
{
	struct path *s = malloc(sizeof(struct path));
	assert(s);
	s->phase = phase;
	s->label = NULL;
	s->inv = NULL;
	return s;
}

struct path *
path_create(struct state *state)
{
	struct path *s = _create(EXEC);
	s->state = state;
	return s;
}

struct path *
path_split(struct path *old, struct rconst *rconst, char *fname)
{
	struct path *new = _create(old->phase);
	switch (old->phase) {
	case EXEC:
	case ATEND:
		new->state = state_split(old->state, rconst, fname);
		break;
	default:
		assert(false);
	}
	return new;
}

void
path_destroy(struct path *s)
{
	/*state_destroy(s->state);*/
	free(s);
}

static char *
phasename(struct path *);

static char *
phase_str(struct path *);

char *
path_str(struct path *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\t%s\n", phasename(s));
	char *phase = phase_str(s);
	strbuilder_printf(b, "%s", phase);
	free(phase);
	return strbuilder_build(b);
}

static char *
_inv_phasename(struct path *s);

static char *
phasename(struct path *s)
{
	switch (s->phase) {
	case EXEC:
		return "EXEC";
	case INV:
		return _inv_phasename(s);
	case ATEND:
		return "END";
	default:
		assert(false);
	}
}

static char *
_inv_phasename(struct path *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "INV");
	if (s->label)
		strbuilder_printf(b, " %s", s->label);
	return strbuilder_build(b);
}

static char *
_state_str(struct state *);

static char *
phase_str(struct path *s)
{
	switch (s->phase) {
	case ATEND:
	case EXEC:
		return _state_str(s->state);
	case INV:
		return _state_str(s->inv);
	default:
		assert(0);
	}
}

static char *
_state_str(struct state *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "\ntext:\n%s\n", state_programtext(s));
	strbuilder_printf(b, "%s\n", state_str(s));
	return strbuilder_build(b);
}

int
path_atend(struct path *s)
{
	switch (s->phase) {
	case ATEND:
	default:
		return 0;
	}
}


/* path_progress */

static struct error *
exec_progress(struct path *, progressor *);

static struct error *
inv_progress(struct path *, progressor *);

struct error *
path_progress(struct path *s, progressor *prog)
{
	switch (s->phase) {
	case EXEC:
		return exec_progress(s, prog);
	case INV:
		return inv_progress(s, prog);
	default:
		assert(false);
	}
}

static struct error *
progressortrace(struct path *, progressor *);

static struct error *
exec_progress(struct path *s, progressor *prog)
{	
	if (state_atend(s->state)) {
		s->phase = ATEND;
		return NULL;
	}
	return progressortrace(s, prog);
}

static struct error *
progressortrace(struct path *s, progressor *prog)
{
	struct error *err = prog(s->state);
	if (err) {
		if (error_to_enterinvariant(err)) {
			assert(s->phase == EXEC);
			s->phase = INV;
			s->inv = state_copy(s->state);
			if (error_enterinvariant_haslabel(err))
				s->label = error_enterinvariant_label(err);
			return NULL;
		}
		return state_stacktrace(s->state, err);
	}
	return NULL;
}

static struct error *
inv_progress(struct path *s, progressor *prog)
{
	assert(0);
}

struct error *
path_verify(struct path *s, struct ast_expr *e)
{
	switch (s->phase) {
	case EXEC:
		return ast_stmt_verify(ast_stmt_create_expr(NULL, e), s->state);
	case ATEND:
		return NULL;
	default:
		assert(false);
	}
}

struct lexememarker *
path_lexememarker(struct path *s)
{
	switch (s->phase) {
	case EXEC:
		return state_lexememarker(s->state);
	case INV:
		return state_lexememarker(s->inv);
	case ATEND:
	default:
		assert(false);
	}
}

struct error *
path_audit(struct path *abstract, struct path *actual)
{
	if (state_hasgarbage(actual->state)) {
		v_printf("%s", state_str(actual->state));
		return error_printf(
			"%s: garbage on heap", state_funcname(actual->state)
		);
	}
	struct error *err;
	if ((err = state_verify_endstate(actual->state, abstract->state))) {
		v_printf("spec:\n%s", state_str(abstract->state));
		v_printf("impl:\n%s", state_str(actual->state));
		return error_printf(
			"%s: %s",
			state_funcname(actual->state),
			error_str(err)
		);
	}
	return NULL;
}
