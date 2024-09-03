#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
#include "util.h"
#include "state.h"
#include "verifier.h"

#include "path.h"

struct path {
	enum path_state {
		PATH_STATE_UNINIT,
		PATH_STATE_SETUPABSTRACT,
		PATH_STATE_ABSTRACT,
		PATH_STATE_HALFWAY,
		PATH_STATE_SETUPACTUAL,
		PATH_STATE_ACTUAL,
		PATH_STATE_AUDIT,
		PATH_STATE_ATEND,
	} state;
	struct state *abstract, *actual;
};

struct path *
path_create()
{
	struct path *s = malloc(sizeof(struct path));
	assert(s);
	s->state = PATH_STATE_UNINIT;
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
	switch (s->state) {
	case PATH_STATE_UNINIT:
		return dynamic_str("path init abstract state");
	case PATH_STATE_SETUPABSTRACT:
		return setupabstract_str(s);
	case PATH_STATE_ABSTRACT:
		return abstract_str(s);
	case PATH_STATE_HALFWAY:
		return dynamic_str("path init actual state");
	case PATH_STATE_SETUPACTUAL:
		return setupactual_str(s);
	case PATH_STATE_ACTUAL:
		return actual_str(s);
	case PATH_STATE_AUDIT:
		return dynamic_str("path audit");
	case PATH_STATE_ATEND:
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
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->abstract));
	strbuilder_printf(b, "%s\n", state_str(s->abstract));
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
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->abstract));
	strbuilder_printf(b, "%s\n", state_str(s->abstract));
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
	return s->state == PATH_STATE_ATEND;
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
	switch (s->state) {
	case PATH_STATE_UNINIT:
		return init_abstract(s, rconst, f, ext);
	case PATH_STATE_HALFWAY:
		return init_actual(s, rconst, f, ext);
	case PATH_STATE_AUDIT:
		return audit(s, f);
	case PATH_STATE_SETUPABSTRACT:
		return progress_setupabstract(s, prog);
	case PATH_STATE_ABSTRACT:
		return progress_abstract(s, prog);
	case PATH_STATE_SETUPACTUAL:
		return progress_setupactual(s, prog);
	case PATH_STATE_ACTUAL:
		return progress_actual(s, prog);
	case PATH_STATE_ATEND:
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
	s->abstract = state_create(frame, rconst, ext);
	if ((err = ast_function_initparams(f, s->abstract))) {
		return err;
	}
	assert(!state_readregister(s->abstract));
	struct frame *setupframe = frame_blockfindsetup_create(
		dynamic_str("setup"),
		ast_function_abstract(f)
	);
	state_pushframe(s->abstract, setupframe);
	s->state = PATH_STATE_SETUPABSTRACT;
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
	s->state = PATH_STATE_SETUPACTUAL;
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
	if (!state_equal(s->actual, s->abstract)) {
		/* unequal states are printed by state_equal so that the user
		 * can see the states with undeclared vars */
		return error_printf(
			"%s: actual and abstract states differ",
			ast_function_name(f)
		);
	}
	s->state = PATH_STATE_ATEND;
	return NULL;
}

static struct error *
progressortrace(struct state *, progressor *);

static struct error *
progress_setupabstract(struct path *s, progressor *prog)
{
	if (state_atsetupend(s->abstract)) {
		s->state = PATH_STATE_ABSTRACT;
		return NULL;
	}
	return progressortrace(s->abstract, prog);
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
	if (state_atend(s->abstract)) {
		s->state = PATH_STATE_HALFWAY;
		return NULL;
	}
	return progressortrace(s->abstract, prog);
}

static struct error *
progress_setupactual(struct path *s, progressor *prog)
{
	if (state_atsetupend(s->actual)) {
		s->state = PATH_STATE_ACTUAL;
		return NULL;
	}
	return progressortrace(s->actual, prog);
}

static struct error *
progress_actual(struct path *s, progressor *prog)
{	
	if (state_atend(s->actual)) {
		s->state = PATH_STATE_AUDIT;
		return NULL;
	}
	return progressortrace(s->actual, prog);
}

struct path *
path_copywithsplit(struct path *old, struct rconst *rconst, char *fname)
{
	struct path *s = path_create();
	s->state = old->state;
	switch (old->state) {
	case PATH_STATE_SETUPABSTRACT:
	case PATH_STATE_ABSTRACT:
		s->abstract = state_split(old->abstract, rconst, fname);
		break;
	case PATH_STATE_SETUPACTUAL:
	case PATH_STATE_ACTUAL:
		s->abstract = state_split(old->abstract, rconst, fname);
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
	switch(s->state) {
	case PATH_STATE_ABSTRACT:
		return ast_stmt_verify(
			ast_stmt_create_expr(NULL, expr), s->abstract
		);
	case PATH_STATE_ACTUAL:
		return ast_stmt_verify(
			ast_stmt_create_expr(NULL, expr), s->actual
		);
	case PATH_STATE_UNINIT:
	case PATH_STATE_HALFWAY:
	case PATH_STATE_AUDIT:
	case PATH_STATE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}

struct lexememarker *
path_lexememarker(struct path *s)
{
	switch (s->state) {
	case PATH_STATE_SETUPABSTRACT:
	case PATH_STATE_ABSTRACT:
		return state_lexememarker(s->abstract);
	case PATH_STATE_SETUPACTUAL:
	case PATH_STATE_ACTUAL:
		return state_lexememarker(s->actual);	
	case PATH_STATE_UNINIT:
	case PATH_STATE_HALFWAY:
	case PATH_STATE_AUDIT:
	case PATH_STATE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}
