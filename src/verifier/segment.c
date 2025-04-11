#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "state.h"
#include "util.h"
#include "verifier.h"

#include "segment.h"

struct segment {
	enum segment_phase {
		INIT,
		SETUP,
		EXEC,
		ATEND,
		ATLOOPEND,
	} phase;
	struct state *state;
};

static struct segment *
_segment_create(enum segment_phase phase)
{
	struct segment *s = malloc(sizeof(struct segment));
	assert(s);
	s->phase = phase;
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

char *
segment_str(struct segment *s, char *pathphase)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\t%s (%s)\n", pathphase, phasename(s));
	switch (s->phase) {
	case INIT:
	case ATEND:
	case ATLOOPEND:
		break;
	case SETUP:
	case EXEC:
		strbuilder_printf(b, "\ntext:\n%s\n", state_programtext(s->state));
		strbuilder_printf(b, "%s\n", state_str(s->state));
		break;
	default:
		assert(false);
	}
	return strbuilder_build(b);
}

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
	case ATEND:
		return "END";
	case ATLOOPEND:
		return "END LOOP";
	default:
		assert(false);
	}
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
setup(struct segment *, progressor *);

static struct error *
exec(struct segment *, progressor *);

struct error *
segment_progress(struct segment *s, progressor *prog)
{
	switch (s->phase) {
	case INIT:
		s->phase = SETUP;
		return NULL;
	case SETUP:
		return setup(s, prog);
	case EXEC:
		return exec(s, prog);
	default:
		assert(false);
	}
}

static struct error *
progressortrace(struct state *, progressor *);

static struct error *
setup(struct segment *s, progressor *prog)
{
	if (state_atsetupend(s->state)) {
		s->phase = EXEC;
		return NULL;
	}
	return progressortrace(s->state, prog);
}

static struct error *
progressortrace(struct state *s, progressor *prog)
{
	struct error *err = prog(s);
	if (err) {
		return state_stacktrace(s, err);
	}
	return NULL;
}

static struct error *
exec(struct segment *s, progressor *prog)
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
	return progressortrace(s->state, prog);
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
