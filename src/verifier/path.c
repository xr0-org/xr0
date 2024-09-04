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
	struct state *s;
};

struct segment *
segment_create()
{
	struct segment *s = malloc(sizeof(struct segment));
	assert(s);
	s->phase = SEGMENT_PHASE_INIT;
	return s;
}

struct path {
	enum path_phase {
		PATH_PHASE_ABSTRACT,
		PATH_PHASE_SETUPACTUAL,
		PATH_PHASE_ACTUAL,
		PATH_PHASE_AUDIT,
		PATH_PHASE_ATEND,
	} phase;
	struct segment *abstract;
	struct state *actual;
};

struct path *
path_create()
{
	struct path *s = malloc(sizeof(struct path));
	assert(s);
	s->phase = PATH_PHASE_ABSTRACT;
	s->abstract = segment_create();
	return s;
}

void
path_destroy(struct path *s)
{
	/*state_destroy(s->abstract);*/
	/*state_destroy(s->actual);*/
	free(s);
}

static char *
setupabstract_str(struct path *);

static char *
abstract_str(struct path *);

static char *
setupactual_str(struct path *);

static char *
actual_str(struct path *);

char *
path_str(struct path *s)
{
	switch (s->phase) {
	case PATH_PHASE_ABSTRACT:
		switch (s->abstract->phase) {
		case SEGMENT_PHASE_INIT:
			return dynamic_str("path init abstract state");
		case SEGMENT_PHASE_SETUP:
			return setupabstract_str(s);
		case SEGMENT_PHASE_EXEC:
			return abstract_str(s);
		case SEGMENT_PHASE_ATEND:
			return dynamic_str("path init actual state");
		default:
			assert(false);
		}
	case PATH_PHASE_SETUPACTUAL:
		return setupactual_str(s);
	case PATH_PHASE_ACTUAL:
		return actual_str(s);
	case PATH_PHASE_AUDIT:
		return dynamic_str("path audit");
	case PATH_PHASE_ATEND:
		return dynamic_str("path at end");
	default:
		assert(false);
	}
}

static char *
abstract_str(struct path *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tABSTRACT\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->abstract->s));
	strbuilder_printf(b, "%s\n", state_str(s->abstract->s));
	return strbuilder_build(b);
}

static char *
actual_str(struct path *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tACTUAL\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->actual));
	strbuilder_printf(b, "%s\n", state_str(s->actual));
	return strbuilder_build(b);
}

static char *
setupabstract_str(struct path *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tSETUP (ABSTRACT)\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->abstract->s));
	strbuilder_printf(b, "%s\n", state_str(s->abstract->s));
	return strbuilder_build(b);
}

static char *
setupactual_str(struct path *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tSETUP (ACTUAL)\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->actual));
	strbuilder_printf(b, "%s\n", state_str(s->actual));
	return strbuilder_build(b);
}

int
path_atend(struct path *s)
{
	return s->phase == PATH_PHASE_ATEND;
}


/* path_progress */

static struct error *
init_abstract(struct path *, struct rconst *, struct ast_function *f,
		struct externals *);

static struct error *
init_actual(struct path *, struct rconst *, struct ast_function *,
		struct externals *);

static struct error *
audit(struct path *, struct ast_function *);

static struct error *
progress_setupabstract(struct path *s, progressor *);

static struct error *
progress_abstract(struct path *s, progressor *);

static struct error *
progress_setupactual(struct path *s, progressor *);

static struct error *
progress_actual(struct path *s, progressor *);

struct error *
path_progress(struct path *s, struct rconst *rconst, struct ast_function *f,
		struct externals *ext,
		progressor *prog)
{
	switch (s->phase) {
	case PATH_PHASE_ABSTRACT:
		switch (s->abstract->phase) {
		case SEGMENT_PHASE_INIT:
			return init_abstract(s, rconst, f, ext);
		case SEGMENT_PHASE_SETUP:
			return progress_setupabstract(s, prog);
		case SEGMENT_PHASE_EXEC:
			return progress_abstract(s, prog);
		case SEGMENT_PHASE_ATEND:
			return init_actual(s, rconst, f, ext);
		default:
			assert(false);
		}
	case PATH_PHASE_SETUPACTUAL:
		return progress_setupactual(s, prog);
	case PATH_PHASE_ACTUAL:
		return progress_actual(s, prog);
	case PATH_PHASE_AUDIT:
		return audit(s, f);
	case PATH_PHASE_ATEND:
	default:
		assert(false);
	}
}

static struct error *
init_abstract(struct path *s, struct rconst *rconst, struct ast_function *f,
		struct externals *ext)
{
	struct error *err;

	struct frame *frame = frame_callabstract_create(
		ast_function_name(f),
		ast_function_abstract(f),
		ast_expr_identifier_create(dynamic_str("base abs")), /* XXX */
		f
	);
	s->abstract->s = state_create(frame, rconst, ext);
	if ((err = ast_function_initparams(f, s->abstract->s))) {
		return err;
	}
	assert(!state_readregister(s->abstract->s));
	struct frame *setupframe = frame_blockfindsetup_create(
		dynamic_str("setup"),
		ast_function_abstract(f)
	);
	state_pushframe(s->abstract->s, setupframe);
	s->abstract->phase = SEGMENT_PHASE_SETUP;
	return NULL;
}

static struct error *
init_actual(struct path *s, struct rconst *rconst, struct ast_function *f,
		struct externals *ext)
{
	struct error *err;

	/* if body empty just apply setup */
	struct frame *frame = frame_callactual_create(
		ast_function_name(f),
		ast_function_body(f),
		ast_expr_identifier_create(dynamic_str("base act")), /* XXX */
		f
	);
	s->actual = state_create(frame, rconst, ext);
	if ((err = ast_function_initparams(f, s->actual))) {
		return err;
	}
	assert(!state_readregister(s->actual));
	struct frame *setupframe = frame_blockfindsetup_create(
		dynamic_str("setup"),
		ast_function_abstract(f)
	);
	state_pushframe(s->actual, setupframe);
	s->phase = PATH_PHASE_SETUPACTUAL;
	return NULL;
}

static struct error *
audit(struct path *s, struct ast_function *f)
{
	if (state_hasgarbage(s->actual)) {
		v_printf("actual: %s", state_str(s->actual));
		return error_printf(
			"%s: garbage on heap", ast_function_name(f)
		);
	}
	if (!state_equal(s->actual, s->abstract->s)) {
		/* unequal states are printed by state_equal so that the user
		 * can see the states with undeclared vars */
		return error_printf(
			"%s: actual and abstract states differ",
			ast_function_name(f)
		);
	}
	s->phase = PATH_PHASE_ATEND;
	return NULL;
}

static struct error *
progressortrace(struct state *, progressor *);

static struct error *
progress_setupabstract(struct path *s, progressor *prog)
{
	if (state_atsetupend(s->abstract->s)) {
		s->abstract->phase = SEGMENT_PHASE_EXEC;
		return NULL;
	}
	return progressortrace(s->abstract->s, prog);
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
progress_abstract(struct path *s, progressor *prog)
{	
	if (state_atend(s->abstract->s)) {
		s->abstract->phase = SEGMENT_PHASE_ATEND;
		return NULL;
	}
	return progressortrace(s->abstract->s, prog);
}

static struct error *
progress_setupactual(struct path *s, progressor *prog)
{
	if (state_atsetupend(s->actual)) {
		s->phase = PATH_PHASE_ACTUAL;
		return NULL;
	}
	return progressortrace(s->actual, prog);
}

static struct error *
progress_actual(struct path *s, progressor *prog)
{	
	if (state_atend(s->actual)) {
		s->phase = PATH_PHASE_AUDIT;
		return NULL;
	}
	return progressortrace(s->actual, prog);
}

struct path *
path_copywithsplit(struct path *old, struct rconst *rconst, char *fname)
{
	struct path *s = path_create();
	s->phase = old->phase;
	s->abstract = segment_create();
	switch (old->phase) {
	case PATH_PHASE_ABSTRACT:
		switch (old->abstract->phase) {
		case SEGMENT_PHASE_SETUP:
		case SEGMENT_PHASE_EXEC:
			s->abstract->s = state_split(old->abstract->s, rconst, fname);
			break;
		default:
			assert(false);
		}
		break;
	case PATH_PHASE_SETUPACTUAL:
	case PATH_PHASE_ACTUAL:
		s->abstract->s = state_split(old->abstract->s, rconst, fname);
		s->actual = state_split(old->actual, rconst, fname);
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
		switch (s->abstract->phase) {
		case SEGMENT_PHASE_EXEC:
			return ast_stmt_verify(
				ast_stmt_create_expr(NULL, expr), s->abstract->s
			);
		case SEGMENT_PHASE_INIT:
		case SEGMENT_PHASE_ATEND:
			return NULL;
		default:
			assert(false);
		}
	case PATH_PHASE_ACTUAL:
		return ast_stmt_verify(
			ast_stmt_create_expr(NULL, expr), s->actual
		);
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
		switch (s->abstract->phase) {
		case SEGMENT_PHASE_SETUP:
		case SEGMENT_PHASE_EXEC:
			return state_lexememarker(s->abstract->s);
		case SEGMENT_PHASE_INIT:
		case SEGMENT_PHASE_ATEND:
			return NULL;
		default:
			assert(false);
		}
	case PATH_PHASE_SETUPACTUAL:
	case PATH_PHASE_ACTUAL:
		return state_lexememarker(s->actual);	
	case PATH_PHASE_AUDIT:
	case PATH_PHASE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}
