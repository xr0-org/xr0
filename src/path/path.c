#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
#include "object.h"
#include "state.h"
#include "path.h"

struct path {
	enum path_state {
		PATH_STATE_UNINIT,
		PATH_STATE_ABSTRACT,
		PATH_STATE_HALFWAY,
		PATH_STATE_ACTUAL,
		PATH_STATE_AUDIT,
		PATH_STATE_SPLIT,
		PATH_STATE_ATEND,
	} path_state;
	struct state *abstract, *actual;
	struct path *p_true, *p_false;

	struct ast_function *f;
	struct externals *ext;
};

struct path *
path_create(struct ast_function *f, struct externals *ext)
{
	struct path *p = calloc(1, sizeof(struct path));
	p->f = ast_function_copy(f);
	p->ext = ext;
	p->path_state = PATH_STATE_UNINIT;
	return p;
}

void
path_destroy(struct path *p)
{
	assert(path_atend(p));

	/*state_destroy(p->abstract);*/
	/*state_destroy(p->actual);*/
	ast_function_destroy(p->f);
	free(p);
}

static struct path *
path_copywithcond(struct path *old, struct ast_expr *cond);

static void
path_split(struct path *p, struct ast_expr *cond)
{
	p->p_true = path_copywithcond(p, cond);
	p->p_false = path_copywithcond(p, ast_expr_inverted_copy(cond, true));
	/* TODO: destroy abstract and actual */
	p->abstract = NULL;
	p->actual = NULL;
	p->path_state = PATH_STATE_SPLIT;
}

static struct ast_function *
copy_withcondname(struct ast_function *, struct ast_expr *cond); 

/* state_assume: return false if contradiction encountered. */
static bool
state_assume(struct state *, struct ast_expr *cond);

static struct path *
path_copywithcond(struct path *old, struct ast_expr *cond)
{
	struct path *p = path_create(copy_withcondname(old->f, cond), old->ext);
	char *fname = ast_function_name(p->f);
	p->path_state = old->path_state;
	switch (old->path_state) {
	case PATH_STATE_ABSTRACT:
		p->abstract = state_copywithname(old->abstract, fname);
		if (!state_assume(p->abstract, cond)) {
			p->path_state = PATH_STATE_ATEND;
		}
		break;
	case PATH_STATE_ACTUAL:
		p->abstract = state_copywithname(old->abstract, fname);
		p->actual = state_copywithname(old->actual, fname);
		if (!state_assume(p->actual, cond)) {
			p->path_state = PATH_STATE_ATEND;
		}
		break;
	default:
		assert(false);
	}
	return p;
}

bool
preresult_iserror(struct preresult *);

bool
preresult_iscontradiction(struct preresult *);

static bool
state_assume(struct state *s, struct ast_expr *cond)
{
	struct preresult *r = ast_expr_assume(cond, s);
	assert(!preresult_iserror(r));
	return !preresult_iscontradiction(r);
}

static char *
split_name(char *name, struct ast_expr *cond);

static struct ast_function *
copy_withcondname(struct ast_function *old, struct ast_expr *cond)
{
	struct ast_function *f = ast_function_copy(old);
	ast_function_setname(f, split_name(ast_function_name(f), cond));
	return f;
}

static char *
split_name(char *name, struct ast_expr *assumption)
{
	struct strbuilder *b = strbuilder_create();
	char *assumption_str = ast_expr_str(assumption);
	strbuilder_printf(b, "%s | %s", name, assumption_str);
	free(assumption_str);
	return strbuilder_build(b);
}

bool
path_atend(struct path *p)
{
	switch (p->path_state) {
	case PATH_STATE_SPLIT:
		return path_atend(p->p_true) && path_atend(p->p_false);
	case PATH_STATE_ATEND:
		return true;
	default:
		return false;
	}
}

static struct error *
path_init_abstract(struct path *p);

static struct error *
path_step_abstract(struct path *p);

static struct error *
path_init_actual(struct path *p);

static struct error *
path_step_actual(struct path *p);

static struct error *
path_step_split(struct path *p);

static struct error *
path_audit(struct path *p);

struct error *
path_step(struct path *p)
{
	switch (p->path_state) {
	case PATH_STATE_UNINIT:
		return path_init_abstract(p);
	case PATH_STATE_ABSTRACT:
		return path_step_abstract(p);
	case PATH_STATE_HALFWAY:
		return path_init_actual(p);
	case PATH_STATE_ACTUAL:
		return path_step_actual(p);
	case PATH_STATE_AUDIT:
		return path_audit(p);
	case PATH_STATE_SPLIT:
		return path_step_split(p);
	case PATH_STATE_ATEND:
	default:
		assert(false);
	}
}

static struct error *
path_init_abstract(struct path *p)
{
	struct frame *f = frame_call_create(
		ast_function_name(p->f),
		ast_function_abstract(p->f),
		ast_function_type(p->f),
		true
	);
	p->abstract = state_create(f, p->ext);
	struct error *err = ast_function_initparams(p->f, p->abstract);
	if (err) {
		return err;
	}
	p->path_state = PATH_STATE_ABSTRACT;
	return NULL;
}

static struct error *
path_init_actual(struct path *p)
{
	struct frame *f = frame_call_create(
		ast_function_name(p->f),
		ast_function_body(p->f),
		ast_function_type(p->f),
		false
	);
	p->actual = state_create_withprops(
		f,
		p->ext,
		state_getprops(p->abstract)
	);
	struct error *err = ast_function_initparams(p->f, p->actual);
	if (err) {
		return err;
	}
	err = ast_function_setupabsexec(p->f, p->actual);
	if (err) {
		return err;
	}
	p->path_state = PATH_STATE_ACTUAL;
	return NULL;
}

static struct error *
path_step_abstract(struct path *p)
{
	if (state_atend(p->abstract)) {
		p->path_state = PATH_STATE_HALFWAY;
		return path_step(p);
	}

	struct error *err = state_step(p->abstract);
	if (!err) {
		return NULL;
	}
	struct error *uc_err = error_to_undecideable_cond(err);
	if (!uc_err) {
		return err;
	}
	path_split(p, error_get_undecideable_cond(uc_err));
	return NULL;
}

static struct error *
path_step_actual(struct path *p)
{
	if (state_atend(p->actual)) {
		p->path_state = PATH_STATE_AUDIT;
		return path_step(p);
	}

	struct error *err = state_step(p->actual);
	if (!err) {
		return NULL;
	}
	struct error *uc_err = error_to_undecideable_cond(err);
	if (!uc_err) {
		return err;
	}
	path_split(p, error_get_undecideable_cond(uc_err));
	return NULL;
}

static struct error *
path_audit(struct path *p)
{
	if (state_hasgarbage(p->actual)) {
		v_printf("actual: %s", state_str(p->actual));
		return error_printf(
			"%s: garbage on heap", ast_function_name(p->f)
		);
	}
	if (!state_equal(p->actual, p->abstract)) {
		/* unequal states are printed by state_equal so that the user
		 * can see the states with undeclared vars */
		return error_printf(
			"%s: actual and abstract states differ",
			ast_function_name(p->f)
		);
	}
	p->path_state = PATH_STATE_ATEND;
	return NULL;
}

static struct error *
path_trystep(struct path *p);

static struct error *
path_step_split(struct path *p)
{
	/* path_atend holds this invariant whenever this function is called */ 
	assert(!path_atend(p->p_true) || !path_atend(p->p_false));

	struct error *err = path_trystep(p->p_true);
	if (err) {
		return err;
	}
	return path_trystep(p->p_false);
}

static struct error *
path_trystep(struct path *p)
{
	return path_atend(p) ? NULL : path_step(p);
}
