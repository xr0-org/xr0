#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
#include "object.h"
#include "state.h"
#include "path.h"
#include "value.h"

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
	
	int branch_index;
	struct path_arr *paths;

	struct ast_function *f;
	struct externals *ext;
};

struct path_arr {
	int n;
	struct path **paths;
};

static struct path_arr *
path_arr_create()
{
	struct path_arr *arr = calloc(1, sizeof(struct path_arr));
	assert(arr);
	return arr;
}

static void
path_arr_destroy(struct path_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		path_destroy(arr->paths[i]);
	}
	free(arr->paths);
	free(arr);
}

static int
path_arr_append(struct path_arr *arr, struct path *p)
{
	arr->paths = realloc(arr->paths, sizeof(struct path_arr) * ++arr->n);
	assert(arr->paths);
	int loc = arr->n-1;
	arr->paths[loc] = p;
	return loc;
}

static bool
path_arr_atend(struct path_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		if (!path_atend(arr->paths[i])) {
			return false;	
		}
	}
	return true;
}

struct path *
path_create(struct ast_function *f, struct externals *ext)
{
	struct path *p = calloc(1, sizeof(struct path));
	p->f = ast_function_copy(f);
	p->ext = ext;
	p->paths = path_arr_create();
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

char *
path_abstract_str(struct path *);

char *
path_actual_str(struct path *);

char *
path_split_str(struct path *);

char *
path_str(struct path *p)
{
	switch (p->path_state) {
	case PATH_STATE_UNINIT:
		return dynamic_str("path init abstract state");
	case PATH_STATE_ABSTRACT:
		return path_abstract_str(p);
	case PATH_STATE_HALFWAY:
		return dynamic_str("path init actual state");
	case PATH_STATE_ACTUAL:
		return path_actual_str(p);
	case PATH_STATE_AUDIT:
		return dynamic_str("path audit");
	case PATH_STATE_SPLIT:
		return path_split_str(p);
	case PATH_STATE_ATEND:
		return dynamic_str("path at end");
	default:
		assert(false);
	}
}

char *
path_abstract_str(struct path *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(
		b, "mode: %s\n", state_execmode_str(state_execmode(p->abstract))
	);
	strbuilder_printf(b, "text:\n%s\n", state_programtext(p->abstract));
	strbuilder_printf(b, "%s\n", state_str(p->abstract));
	return strbuilder_build(b);
}

char *
path_actual_str(struct path *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(
		b, "mode: %s\n", state_execmode_str(state_execmode(p->actual))
	);
	strbuilder_printf(b, "text:\n%s\n", state_programtext(p->actual));
	strbuilder_printf(b, "%s\n", state_str(p->actual));
	return strbuilder_build(b);
}

char *
path_split_str(struct path *p)
{
	struct path *branch = p->paths->paths[p->branch_index];
	return path_str(branch);
}

static void
path_nextbranch(struct path *p)
{
	assert(p->paths->n >= 2);
	int index = p->branch_index;
	if (index < p->paths->n - 1) {
		p->branch_index++;	
	}
}

bool
path_atend(struct path *p)
{
	switch (p->path_state) {
	case PATH_STATE_SPLIT:
		return path_arr_atend(p->paths);
	case PATH_STATE_ATEND:
		return true;
	default:
		return false;
	}
}


/* path_step */

static struct error *
path_init_abstract(struct path *p);

static struct error *
path_step_abstract(struct path *p, bool print);

static struct error *
path_init_actual(struct path *p);

static struct error *
path_step_actual(struct path *p, bool print);

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
		return path_step_abstract(p, true);
	case PATH_STATE_HALFWAY:
		return path_init_actual(p);
	case PATH_STATE_ACTUAL:
		return path_step_actual(p, true);
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
	struct error *err;

	struct frame *f = frame_call_create(
		ast_function_name(p->f),
		ast_function_abstract(p->f),
		ast_function_type(p->f),
		EXEC_ABSTRACT,
		ast_expr_identifier_create(dynamic_str("base abs")), /* XXX */
		p->f
	);
	p->abstract = state_create(f, p->ext);
	if ((err = ast_function_initparams(p->f, p->abstract))) {
		return err;
	}
	state_clearregister(p->abstract);
	p->path_state = PATH_STATE_ABSTRACT;
	return NULL;
}

static void
path_split(struct path *p, struct ast_expr *cond);

static struct error *
path_step_abstract(struct path *p, bool print)
{	
	if (state_atend(p->abstract) && state_frameid(p->abstract) == 0) {
		p->path_state = PATH_STATE_HALFWAY;
		return path_step(p);
	}

	struct error *err = state_step(p->abstract);
	if (!err) {
		return NULL;
	}
	struct error *uc_err = error_to_undecideable_cond(err);
	if (uc_err) {
		path_split(p, error_get_undecideable_cond(uc_err));
		return NULL;
	}
	return state_stacktrace(p->abstract, err);
}

static struct path *
path_copywithcond(struct path *old, struct ast_expr *cond);

static void
path_split(struct path *p, struct ast_expr *cond)
{
	path_arr_append(p->paths, path_copywithcond(p, cond));
	path_arr_append(p->paths, path_copywithcond(p, ast_expr_inverted_copy(cond, true)));
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



static struct error *
path_init_actual(struct path *p)
{
	struct error *err;
	/* if body empty just apply setup */
	struct frame *f = frame_call_create(
		ast_function_name(p->f),
		ast_function_body(p->f),
		ast_function_type(p->f),
		EXEC_ACTUAL,
		ast_expr_identifier_create(dynamic_str("base act")), /* xxx */
		p->f
	);
	p->actual = state_create_withprops(
		f,
		p->ext,
		state_getprops(p->abstract)
	);
	if ((err = ast_function_initparams(p->f, p->actual))) {
		return err;
	}
	if ((err = ast_function_initsetup(p->f, p->actual))) {
		return err;
	}
	state_clearregister(p->actual);
	p->path_state = PATH_STATE_ACTUAL;
	return NULL;
}

static struct error *
path_step_actual(struct path *p, bool print)
{	
	if (state_atend(p->actual) && state_frameid(p->actual) == 0) {
		p->path_state = PATH_STATE_AUDIT;
		return path_step(p);
	}

	struct error *err = state_step(p->actual);
	if (!err) {
		return NULL;
	}
	struct error *uc_err = error_to_undecideable_cond(err);
	if (uc_err) {
		path_split(p, error_get_undecideable_cond(uc_err));
		return NULL;
	}
	return state_stacktrace(p->actual, err);
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
branch_step(struct path *parent, struct path *branch);

static struct error *
path_step_split(struct path *p)
{
	/* path_atend holds this invariant whenever this function is called */ 
	assert(!path_arr_atend(p->paths));

	struct path_arr *p_arr = p->paths;
	struct error *err = branch_step(p, p_arr->paths[p->branch_index]);
	if (err) {
		return err;
	}
	return NULL;	
}

static struct error *
branch_step(struct path *parent, struct path *branch)
{
	d_printf("branch: %d\n", parent->branch_index);
	if (path_atend(branch)) {
		path_nextbranch(parent);
		return NULL;
	}
	return path_step(branch);
}


/* path_next */

static struct error *
path_next_abstract(struct path *);

static struct error *
path_next_actual(struct path *);

static struct error *
path_next_split(struct path *);

struct error *
path_next(struct path *p)
{
	switch (p->path_state) {
	case PATH_STATE_UNINIT:
		return path_step(p);
	case PATH_STATE_ABSTRACT:
		return path_next_abstract(p);
	case PATH_STATE_HALFWAY:
		return path_step(p);
	case PATH_STATE_ACTUAL:
		return path_next_actual(p);
	case PATH_STATE_AUDIT:
		return path_audit(p);
	case PATH_STATE_SPLIT:
		return path_next_split(p);
	case PATH_STATE_ATEND:
	default:
		assert(false);
	}
}

static struct error *
path_next_abstract(struct path *p)
{
	if (state_atend(p->abstract) && state_frameid(p->abstract) == 0) {
		p->path_state = PATH_STATE_HALFWAY;
		return path_step(p);
	}
	struct error *err = state_next(p->abstract);
	if (!err) {
		return NULL;
	}
	struct error *uc_err = error_to_undecideable_cond(err);
	if (uc_err) {
		path_split(p, error_get_undecideable_cond(uc_err));
		return NULL;
	}
	return state_stacktrace(p->abstract, err);
}

static struct error *
path_next_actual(struct path *p)
{
	if (state_atend(p->actual) && state_frameid(p->actual) == 0) {
		p->path_state = PATH_STATE_AUDIT;
		return path_step(p);
	}
	struct error *err = state_next(p->actual);
	if (!err) {
		return NULL;
	}
	struct error *uc_err = error_to_undecideable_cond(err);
	if (uc_err) {
		path_split(p, error_get_undecideable_cond(uc_err));
		return NULL;
	}
	return state_stacktrace(p->actual, err);
}

static struct error *
branch_next(struct path *parent, struct path *branch);

static struct error *
path_next_split(struct path *p)
{
	assert(!path_arr_atend(p->paths));
	struct path_arr *p_arr = p->paths;
	struct error *err = branch_next(p, p_arr->paths[p->branch_index]);
	if (err) {
		return err;
	}
	return NULL;
}

static struct error *
branch_next(struct path *parent, struct path *branch)
{
	d_printf("branch: %d\n", parent->branch_index);
	if (path_atend(branch)) {
		path_nextbranch(parent);
		return NULL;
	}
	return path_next(branch);
}

static struct error *
path_split_verify(struct path *, struct ast_expr *);

struct error *
path_verify(struct path *p, struct ast_expr *expr)
{
	switch(p->path_state) {
	case PATH_STATE_ABSTRACT:
		return ast_stmt_verify(ast_stmt_create_expr(NULL, expr), p->abstract);
	case PATH_STATE_ACTUAL:
		return ast_stmt_verify(ast_stmt_create_expr(NULL, expr), p->actual);	
	case PATH_STATE_SPLIT:
		return path_split_verify(p, expr);
	case PATH_STATE_UNINIT:
	case PATH_STATE_HALFWAY:
	case PATH_STATE_AUDIT:
	case PATH_STATE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}

static struct error *
path_split_verify(struct path *p, struct ast_expr *expr)
{
	struct path *branch = p->paths->paths[p->branch_index];
	return path_verify(branch, expr);
}

static struct lexememarker *
path_split_lexememarker(struct path *);

struct lexememarker *
path_lexememarker(struct path *p)
{
	switch (p->path_state) {
	case PATH_STATE_ABSTRACT:
		return state_lexememarker(p->abstract);
	case PATH_STATE_ACTUAL:
		return state_lexememarker(p->actual);	
	case PATH_STATE_SPLIT:
		return path_split_lexememarker(p);
	case PATH_STATE_UNINIT:
	case PATH_STATE_HALFWAY:
	case PATH_STATE_AUDIT:
	case PATH_STATE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}

static struct lexememarker *
path_split_lexememarker(struct path *p)
{
	struct path *branch = p->paths->paths[p->branch_index];
	return path_lexememarker(branch);
}
