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
		SEGMENT_PHASE_INIT,
		SEGMENT_PHASE_SETUP,
		SEGMENT_PHASE_EXEC,
		SEGMENT_PHASE_ATEND,
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
	struct segment *s = _segment_create(SEGMENT_PHASE_INIT);
	s->state = state;
	return s;
}

struct segment *
segment_split(struct segment *old, struct rconst *rconst, char *fname)
{
	struct segment *new = _segment_create(old->phase);
	switch (old->phase) {
	case SEGMENT_PHASE_INIT:
		assert(old->state);
		/* fallthrough */
	case SEGMENT_PHASE_SETUP:
	case SEGMENT_PHASE_EXEC:
	case SEGMENT_PHASE_ATEND:
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
	case SEGMENT_PHASE_INIT:
	case SEGMENT_PHASE_ATEND:
		break;
	case SEGMENT_PHASE_SETUP:
	case SEGMENT_PHASE_EXEC:
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
	case SEGMENT_PHASE_INIT:
		return "INIT";
	case SEGMENT_PHASE_SETUP:
		return "SETUP";
	case SEGMENT_PHASE_EXEC:
		return "EXEC";
	case SEGMENT_PHASE_ATEND:
		return "END";
	default:
		assert(false);
	}
}

int
segment_atend(struct segment *s)
{
	return s->phase == SEGMENT_PHASE_ATEND;
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
	case SEGMENT_PHASE_INIT:
		s->phase = SEGMENT_PHASE_SETUP;
		return NULL;
	case SEGMENT_PHASE_SETUP:
		return setup(s, prog);
	case SEGMENT_PHASE_EXEC:
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
		s->phase = SEGMENT_PHASE_EXEC;
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
		assert(false);
	}
	if (state_atend(s->state)) {
		s->phase = SEGMENT_PHASE_ATEND;
		return NULL;
	}
	return progressortrace(s->state, prog);
}

struct error *
segment_verify(struct segment *s, struct ast_expr *e)
{
	switch (s->phase) {
	case SEGMENT_PHASE_EXEC:
		return ast_stmt_verify(ast_stmt_create_expr(NULL, e), s->state);
	case SEGMENT_PHASE_INIT:
	case SEGMENT_PHASE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}

struct lexememarker *
segment_lexememarker(struct segment *s)
{
	switch (s->phase) {
	case SEGMENT_PHASE_SETUP:
	case SEGMENT_PHASE_EXEC:
		return state_lexememarker(s->state);
	case SEGMENT_PHASE_INIT:
	case SEGMENT_PHASE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}

struct error *
segment_audit(struct segment *abstract, struct segment *actual)
{
	if (state_hasgarbage(actual->state)) {
		v_printf("actual: %s", state_str(actual->state));
		return error_printf(
			"%s: garbage on heap", state_funcname(actual->state)
		);
	}
	struct error *err;
	if ((err = state_specverify(actual->state, abstract->state))) {
		v_printf("actual:\n%s", state_str(actual->state));
		v_printf("abstract:\n%s", state_str(abstract->state));
		return error_printf(
			"%s: %s",
			state_funcname(actual->state),
			error_str(err)
		);
	}
	return NULL;
}
