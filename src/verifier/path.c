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

struct segment *
segment_create()
{
	struct segment *s = malloc(sizeof(struct segment));
	assert(s);
	s->phase = SEGMENT_PHASE_INIT;
	return s;
}

struct segment *
segment_copywithsplit(struct segment *old, struct rconst *rconst, char *fname)
{
	struct segment *new = segment_create();
	new->phase = old->phase;
	switch (old->phase) {
	case SEGMENT_PHASE_SETUP:
	case SEGMENT_PHASE_EXEC:
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

struct path *
path_create()
{
	struct path *s = malloc(sizeof(struct path));
	assert(s);
	s->phase = PATH_PHASE_ABSTRACT;
	s->abstract = segment_create();
	s->actual = segment_create();
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
init(struct segment *, struct rconst *, struct ast_function *f,
		struct externals *, int abstract);

static struct error *
progress_setupabstract(struct segment *, progressor *);

static struct error *
progress_abstract(struct segment *, progressor *);

static struct error *
audit(struct path *, struct ast_function *);

struct error *
path_progress(struct path *p, struct rconst *rconst, struct ast_function *f,
		struct externals *ext, progressor *prog)
{
	switch (p->phase) {
	case PATH_PHASE_ABSTRACT:
		if (p->abstract->phase == SEGMENT_PHASE_ATEND) {
			p->phase = PATH_PHASE_ACTUAL;
			return NULL;
		}
		switch (p->abstract->phase) {
		case SEGMENT_PHASE_INIT:
			return init(p->abstract, rconst, f, ext, 1);
		case SEGMENT_PHASE_SETUP:
			return progress_setupabstract(p->abstract, prog);
		case SEGMENT_PHASE_EXEC:
			return progress_abstract(p->abstract, prog);
		default:
			assert(false);
		}
	case PATH_PHASE_ACTUAL:
		if (p->actual->phase == SEGMENT_PHASE_ATEND) {
			p->phase = PATH_PHASE_AUDIT;
			return NULL;
		}
		switch (p->actual->phase) {
		case SEGMENT_PHASE_INIT:
			return init(p->actual, rconst, f, ext, 0);
		case SEGMENT_PHASE_SETUP:
			return progress_setupabstract(p->actual, prog);
		case SEGMENT_PHASE_EXEC:
			return progress_abstract(p->actual, prog);
		case SEGMENT_PHASE_ATEND:
			p->phase = PATH_PHASE_AUDIT;
			return NULL;
		default:
			assert(false);
		}
	case PATH_PHASE_AUDIT:
		return audit(p, f);
	case PATH_PHASE_ATEND:
	default:
		assert(false);
	}
}

static struct state *
init_state(struct rconst *, struct ast_function *, struct externals *,
		int abstract);

static struct error *
init(struct segment *s, struct rconst *rconst, struct ast_function *f,
		struct externals *ext, int abstract)
{
	struct error *err;
	
	s->state = init_state(rconst, f, ext, abstract);
	if ((err = ast_function_initparams(f, s->state))) {
		return err;
	}
	assert(!state_readregister(s->state));
	struct frame *setupframe = frame_blockfindsetup_create(
		dynamic_str("setup"),
		ast_function_abstract(f)
	);
	state_pushframe(s->state, setupframe);
	s->phase = SEGMENT_PHASE_SETUP;
	return NULL;
}

static struct state *
init_state(struct rconst *rconst, struct ast_function *f, struct externals *ext,
		int abstract)
{
	struct ast_expr *frame_id = ast_expr_identifier_create(
		abstract ? dynamic_str("base abs") : dynamic_str("base act") /* XXX */
	);
	struct frame *frame = frame_callabstract_create(
		ast_function_name(f),
		abstract ? ast_function_abstract(f) : ast_function_body(f),
		frame_id,
		f
	);
	return state_create(frame, rconst, ext);
}


static struct error *
audit(struct path *p, struct ast_function *f)
{
	if (state_hasgarbage(p->actual->state)) {
		v_printf("actual: %s", state_str(p->actual->state));
		return error_printf(
			"%s: garbage on heap", ast_function_name(f)
		);
	}
	if (!state_equal(p->actual->state, p->abstract->state)) {
		/* unequal states are printed by state_equal so that the user
		 * can see the states with undeclared vars */
		return error_printf(
			"%s: actual and abstract states differ",
			ast_function_name(f)
		);
	}
	p->phase = PATH_PHASE_ATEND;
	return NULL;
}

static struct error *
progressortrace(struct state *, progressor *);

static struct error *
progress_setupabstract(struct segment *s, progressor *prog)
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
progress_abstract(struct segment *s, progressor *prog)
{	
	if (state_atend(s->state)) {
		s->phase = SEGMENT_PHASE_ATEND;
		return NULL;
	}
	return progressortrace(s->state, prog);
}

struct path *
path_copywithsplit(struct path *old, struct rconst *rconst, char *fname)
{
	struct path *s = path_create();
	s->phase = old->phase;
	switch (old->phase) {
	case PATH_PHASE_ABSTRACT:
		s->abstract = segment_copywithsplit(old->abstract, rconst, fname);
		break;
	case PATH_PHASE_ACTUAL:
		s->abstract->state = state_split(old->abstract->state, rconst, fname);
		s->actual->state = state_split(old->actual->state, rconst, fname);
		break;
	default:
		assert(false);
	}
	return s;
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
