#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "state.h"
#include "util.h"
#include "verifier.h"
#include "inv_verifier.h"

#include "segment.h"

struct segment {
	enum phase { INIT, SETUP, EXEC, INV, ATEND, ATLOOPEND, } phase;

	struct state *state;

	struct inv_verifier *iv;
	char *label; /* label of invariant under consideration */
};

static struct segment *
_segment_create(enum phase phase)
{
	struct segment *s = malloc(sizeof(struct segment));
	assert(s);
	s->phase = phase;
	s->label = NULL;
	s->iv = NULL;
	return s;
}

struct segment *
segment_create_withstate(struct state *state)
{
	struct segment *s = _segment_create(INIT);
	s->state = state;
	return s;
}

struct segment *
segment_split(struct segment *old, struct rconst *rconst, char *fname)
{
	struct segment *new = _segment_create(old->phase);
	switch (old->phase) {
	case INIT:
		assert(old->state);
		/* fallthrough */
	case SETUP:
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
segment_destroy(struct segment *s)
{
	/*state_destroy(s->state);*/
	free(s);
}

static char *
phasename(struct segment *);

static char *
phase_str(struct segment *);

char *
segment_str(struct segment *s, char *pathphase)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\t%s — %s\n", pathphase, phasename(s));
	char *phase = phase_str(s);
	strbuilder_printf(b, "%s", phase);
	free(phase);
	return strbuilder_build(b);
}

static char *
_inv_phasename(struct segment *s);

static char *
phasename(struct segment *s)
{
	switch (s->phase) {
	case INIT:
		return "INIT";
	case SETUP:
		return "SETUP";
	case EXEC:
		return "EXEC";
	case INV:
		return _inv_phasename(s);
	case ATEND:
		return "END";
	case ATLOOPEND:
		return "END LOOP";
	default:
		assert(false);
	}
}

static char *
_inv_phasename(struct segment *s)
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
_inv_str(struct inv_verifier *);

static char *
phase_str(struct segment *s)
{
	switch (s->phase) {
	case INIT:
	case ATEND:
	case ATLOOPEND:
		return dynamic_str("");
	case SETUP:
	case EXEC:
		return _state_str(s->state);
	case INV:
		return _inv_str(s->iv);
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

static char *
_inv_str(struct inv_verifier *iv)
{
	if (inv_verifier_atend(iv)) {
		return dynamic_str("");
	}
	struct strbuilder *b = strbuilder_create();
	char *s = inv_verifier_str(iv);
	strbuilder_printf(b, "%s", s);
	free(s);
	return strbuilder_build(b);
}


int
segment_atend(struct segment *s)
{
	switch (s->phase) {
	case ATEND:
	case ATLOOPEND:
		return 1;
	default:
		return 0;
	}
}


/* segment_progress */

static struct error *
setup_progress(struct segment *, progressor *);

static struct error *
exec_progress(struct segment *, progressor *);

static struct error *
inv_progress(struct segment *, progressor *);

struct error *
segment_progress(struct segment *s, progressor *prog)
{
	switch (s->phase) {
	case INIT:
		s->phase = SETUP;
		return NULL;
	case SETUP:
		return setup_progress(s, prog);
	case EXEC:
		return exec_progress(s, prog);
	case INV:
		return inv_progress(s, prog);
	default:
		assert(false);
	}
}

static struct error *
progressortrace(struct segment *, progressor *);

static struct error *
setup_progress(struct segment *s, progressor *prog)
{
	if (state_atsetupend(s->state)) {
		s->phase = EXEC;
		return NULL;
	}
	return progressortrace(s, prog);
}

static struct error *
progressortrace(struct segment *s, progressor *prog)
{
	struct error *err = prog(s->state);
	if (err) {
		if (error_to_enterinvariant(err)) {
			assert(s->phase == EXEC);
			s->phase = INV;
			s->iv = inv_verifier_create(state_copy(s->state));
			if (error_enterinvariant_haslabel(err))
				s->label = error_enterinvariant_label(err);
			return NULL;
		}
		return state_stacktrace(s->state, err);
	}
	return NULL;
}

static struct error *
exec_progress(struct segment *s, progressor *prog)
{	
	if (state_atloopend(s->state)) {
		struct error *err = state_verify_invariant(s->state);
		if (err) {
			return state_stacktrace(s->state, err);
		}
		s->phase = ATLOOPEND;
	}
	if (state_atend(s->state)) {
		s->phase = ATEND;
		return NULL;
	}
	return progressortrace(s, prog);
}

static struct error *
inv_progress(struct segment *s, progressor *prog)
{
	if (inv_verifier_atend(s->iv)) {
		struct error *err = inv_verifier_verify(s->iv, s->state);
		if (err) {
			return state_stacktrace(
				s->state,
				error_printf("invariant: %w", err)
			);
		}
		/* TODO: store these states against s->label in some kind of
		 * map for future reference */
		assert(0);
	}
	return inv_verifier_progress(s->iv, prog);
}

struct error *
segment_verify(struct segment *s, struct ast_expr *e)
{
	switch (s->phase) {
	case EXEC:
		return ast_stmt_verify(ast_stmt_create_expr(NULL, e), s->state);
	case INIT:
	case ATEND:
		return NULL;
	default:
		assert(false);
	}
}

struct lexememarker *
segment_lexememarker(struct segment *s)
{
	switch (s->phase) {
	case SETUP:
	case EXEC:
		return state_lexememarker(s->state);
	case INV:
		if (inv_verifier_atend(s->iv)) {
			return NULL;
		}
		return inv_verifier_lexememarker(s->iv);
	case INIT:
	case ATEND:
	case ATLOOPEND:
		return NULL;
	default:
		assert(false);
	}
}

struct error *
segment_audit(struct segment *abstract, struct segment *actual)
{
	if (actual->phase == ATLOOPEND) {
		return NULL;
	}

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
