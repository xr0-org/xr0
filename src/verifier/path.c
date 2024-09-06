#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
#include "state.h"
#include "util.h"
#include "verifier.h"

#include "path.h"

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
segment_copywithsplit(struct segment *old, struct rconst *rconst, char *fname)
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

static char *
setup_str(struct segment *, int abstract);

static char *
exec_str(struct segment *, int abstract);

char *
segment_str(struct segment *s, int abstract)
{
	switch (s->phase) {
	case SEGMENT_PHASE_INIT:
		return abstract
			? dynamic_str("path init abstract state")
			: dynamic_str("path init actual state");
	case SEGMENT_PHASE_SETUP:
		return setup_str(s, abstract);
	case SEGMENT_PHASE_EXEC:
		return exec_str(s, abstract);
	case SEGMENT_PHASE_ATEND:
		return dynamic_str("");
	default:
		assert(false);
	}
}

static char *
setup_str(struct segment *s, int abstract)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(
		b, "phase:\tSETUP (%s)\n\n", abstract ? "ABSTRACT" : "ACTUAL"
	);
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->state));
	strbuilder_printf(b, "%s\n", state_str(s->state));
	return strbuilder_build(b);
}

static char *
exec_str(struct segment *s, int abstract)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(
		b, "phase:\t%s\n\n", abstract ? "ABSTRACT" : "ACTUAL"
	);
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->state));
	strbuilder_printf(b, "%s\n", state_str(s->state));
	return strbuilder_build(b);
}

int
segment_atend(struct segment *s)
{
	return s->phase == SEGMENT_PHASE_ATEND;
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


struct path {
	enum path_phase {
		PATH_PHASE_ABSTRACT,
		PATH_PHASE_ACTUAL,
		PATH_PHASE_AUDIT,
		PATH_PHASE_ATEND,
	} phase;
	struct segment *abstract, *actual;
};

static struct state *
state_abstract(struct rconst *, struct ast_function *, struct externals *);

static struct state *
state_actual(struct rconst *, struct ast_function *, struct externals *);

struct path *
path_create(struct rconst *rconst, struct ast_function *f, struct externals *ext)
{
	struct path *s = malloc(sizeof(struct path));
	assert(s);
	s->phase = PATH_PHASE_ABSTRACT;
	s->abstract = segment_create_withstate(state_abstract(rconst, f, ext));
	s->actual = segment_create_withstate(state_actual(rconst, f, ext));
	return s;
}

static struct frame *
frame_setup(struct ast_function *);

static struct state *
state_abstract(struct rconst *rconst, struct ast_function *f,
		struct externals *ext)
{
	struct state *s = state_create(
		frame_callabstract_create(
			ast_function_name(f),
			ast_function_abstract(f),
			/* XXX */
			ast_expr_identifier_create(dynamic_str("base abs")),
			f
		),
		rconst, ext
	);
	ast_function_initparams(f, s);
	state_pushframe(s, frame_setup(f));
	return s;
}

static struct frame *
frame_setup(struct ast_function *f)
{
	return frame_blockfindsetup_create(
		dynamic_str("setup"), ast_function_abstract(f)
	);
}

static struct state *
state_actual(struct rconst *rconst, struct ast_function *f,
		struct externals *ext)
{
	struct state *s = state_create(
		frame_callactual_create(
			ast_function_name(f),
			ast_function_body(f),
			/* XXX */
			ast_expr_identifier_create(dynamic_str("base act")),
			f
			
		), rconst, ext
	);
	ast_function_initparams(f, s);
	state_pushframe(s, frame_setup(f));
	return s;
}

struct path *
path_copywithsplit(struct path *old, struct rconst *rconst, char *fname)
{
	struct path *s = malloc(sizeof(struct path));
	assert(s);
	s->phase = old->phase;
	switch (old->phase) {
	case PATH_PHASE_ABSTRACT:
	case PATH_PHASE_ACTUAL:
		s->abstract = segment_copywithsplit(old->abstract, rconst, fname);
		s->actual = segment_copywithsplit(old->actual, rconst, fname);
		break;
	default:
		assert(false);
	}
	return s;
}

void
path_destroy(struct path *s)
{
	/*state_destroy(s->abstract);*/
	/*state_destroy(s->actual);*/
	free(s);
}

char *
path_str(struct path *s)
{
	switch (s->phase) {
	case PATH_PHASE_ABSTRACT:
		return segment_str(s->abstract, 1);
	case PATH_PHASE_ACTUAL:
		return segment_str(s->actual, 0);
	case PATH_PHASE_AUDIT:
		return dynamic_str("path audit");
	case PATH_PHASE_ATEND:
		return dynamic_str("path at end");
	default:
		assert(false);
	}
}

int
path_atend(struct path *s)
{
	return s->phase == PATH_PHASE_ATEND;
}


/* path_progress */

static struct error *
setup(struct segment *, progressor *);

static struct error *
exec(struct segment *, progressor *);

static struct error *
audit(struct path *);

struct error *
path_progress(struct path *p, progressor *prog)
{
	switch (p->phase) {
	case PATH_PHASE_ABSTRACT:
		if (segment_atend(p->abstract)) {
			p->phase = PATH_PHASE_ACTUAL;
			return path_progress(p, prog);
		}
		switch (p->abstract->phase) {
		case SEGMENT_PHASE_INIT:
			p->abstract->phase = SEGMENT_PHASE_SETUP;
			return NULL;
		case SEGMENT_PHASE_SETUP:
			return setup(p->abstract, prog);
		case SEGMENT_PHASE_EXEC:
			return exec(p->abstract, prog);
		default:
			assert(false);
		}
	case PATH_PHASE_ACTUAL:
		if (segment_atend(p->actual)) {
			p->phase = PATH_PHASE_AUDIT;
			return path_progress(p, prog);
		}
		switch (p->actual->phase) {
		case SEGMENT_PHASE_INIT:
			p->actual->phase = SEGMENT_PHASE_SETUP;
			return NULL;
		case SEGMENT_PHASE_SETUP:
			return setup(p->actual, prog);
		case SEGMENT_PHASE_EXEC:
			return exec(p->actual, prog);
		default:
			assert(false);
		}
	case PATH_PHASE_AUDIT:
		return audit(p);
	case PATH_PHASE_ATEND:
	default:
		assert(false);
	}
}

static struct error *
audit(struct path *p)
{
	if (state_hasgarbage(p->actual->state)) {
		v_printf("actual: %s", state_str(p->actual->state));
		return error_printf(
			"%s: garbage on heap", state_funcname(p->actual->state)
		);
	}
	if (!state_equal(p->actual->state, p->abstract->state)) {
		/* unequal states are printed by state_equal so that the user
		 * can see the states with undeclared vars */
		return error_printf(
			"%s: actual and abstract states differ",
			state_funcname(p->actual->state)
		);
	}
	p->phase = PATH_PHASE_ATEND;
	return NULL;
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
	if (state_atend(s->state)) {
		s->phase = SEGMENT_PHASE_ATEND;
		return NULL;
	}
	return progressortrace(s->state, prog);
}

struct error *
path_verify(struct path *s, struct ast_expr *expr)
{
	switch (s->phase) {
	case PATH_PHASE_ABSTRACT:
		return segment_verify(s->abstract, expr);
	case PATH_PHASE_ACTUAL:
		return segment_verify(s->actual, expr);
	case PATH_PHASE_AUDIT:
	case PATH_PHASE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}

struct lexememarker *
path_lexememarker(struct path *s)
{
	switch (s->phase) {
	case PATH_PHASE_ABSTRACT:
		return segment_lexememarker(s->abstract);
	case PATH_PHASE_ACTUAL:
		return segment_lexememarker(s->actual);
	case PATH_PHASE_AUDIT:
	case PATH_PHASE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}
