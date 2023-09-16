#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "state.h"
#include "util.h"
#include "verify.h"

typedef struct func_arr *Funcarr;

int
func_arr_len(Funcarr);

struct ast_function **
func_arr_paths(Funcarr);

void
func_arr_destroy(Funcarr);

Funcarr
paths_fromfunction(struct ast_function *f);

struct error *
path_verify(struct ast_function *f, struct state *state);

struct error *
function_verify(struct ast_function *f, struct map *extfunc)
{
	struct error *err;

	Funcarr paths = paths_fromfunction(f);
	int len = func_arr_len(paths);
	struct ast_function **path = func_arr_paths(paths);
	for (int i = 0; i < len; i++) {
		struct state *state = state_create(extfunc, ast_function_type(f));
		if ((err = path_verify(path[i], state))) {
			return err;
		}
		state_destroy(state);
	}
	func_arr_destroy(paths);
	return NULL;
}

/* Funcarr */

struct func_arr {
	int n;
	struct ast_function **path;
};

struct func_arr *
func_arr_create()
{
	return calloc(1, sizeof(struct func_arr));
}

int
func_arr_len(struct func_arr *set)
{
	return set->n;
}

struct ast_function **
func_arr_paths(struct func_arr *set)
{
	return set->path;
}

static int
findsel(struct ast_function *);

static void
func_arr_immediate_split_append(struct func_arr *set, struct ast_function *f, int i);

static void
func_arr_append(struct func_arr *set, struct ast_function *f);

struct func_arr *
paths_fromfunction(struct ast_function *f)
{
	assert(!ast_function_isaxiom(f));

	struct func_arr *arr = func_arr_create();

	int sel = findsel(f);
	if (sel != -1) {
		func_arr_immediate_split_append(arr, f, sel);
	} else {
		func_arr_append(arr, f);
	}
	return arr;
}

static int
findsel(struct ast_function *f)
{
	assert(!ast_function_isaxiom(f));

	struct ast_block *body = ast_function_body(f);

	struct ast_stmt **stmt = ast_block_stmts(body);
	int nstmts = ast_block_nstmts(body);

	for (int i = 0; i < nstmts; i++) {
		struct ast_stmt *s = stmt[i];
		if (ast_stmt_kind(s) != STMT_SELECTION) {
			continue;
		}
		assert(!ast_stmt_sel_nest(s)); /* XXX: simple cases first */
		return i;
	}
	return -1;
}

/* func_arr_immediate_split_append */

static struct ast_function *
immediate_split(struct ast_function *f, int i, bool enter);

static void
func_arr_appendrange(struct func_arr *set, struct func_arr *other);

static void
func_arr_immediate_split_append(struct func_arr *set, struct ast_function *f, int i)
{
	struct ast_function *t_path = immediate_split(f, i, true),
			    *f_path = immediate_split(f, i, false);

	struct func_arr *paths_t = paths_fromfunction(t_path),
			*paths_f = paths_fromfunction(f_path);

	func_arr_appendrange(set, paths_t);
	func_arr_appendrange(set, paths_f);

	func_arr_destroy(paths_f);
	func_arr_destroy(paths_t);

	ast_function_destroy(f_path);
	ast_function_destroy(t_path);
}

struct ast_block *
split_block_cond(struct ast_block *b, struct ast_expr *cond, bool enter);

struct ast_block *
split_block_index(struct ast_block *b, int split_index, bool enter);

static struct ast_function *
immediate_split(struct ast_function *f, int split_index, bool enter)
{
	struct ast_block *abs = ast_function_abstract(f),
			 *body = ast_function_body(f);

	struct ast_expr *cond = ast_stmt_sel_cond(ast_block_stmts(body)[split_index]);

	assert(!abs->decl && abs->ndecl == 0);
	return ast_function_create(
		false,
		ast_type_copy(f->ret),
		dynamic_str(f->name),
		f->nparam,
		ast_variables_copy(f->nparam, f->param),
		split_block_cond(abs, cond, enter),
		split_block_index(body, split_index, enter)
	);
}

/* split_block_cond */

struct ast_stmt *
choose_split_path(struct ast_stmt *old_stmt, bool should_split, bool enter);

struct ast_block *
split_block_cond(struct ast_block *b, struct ast_expr *cond, bool enter)
{
	int nstmts = ast_block_nstmts(b);
	struct ast_stmt **old_stmt = ast_block_stmts(b);

	int n = 0;
	struct ast_stmt **stmt = NULL;
	for (int i = 0; i < nstmts; i++) {
		struct ast_stmt *old_s = old_stmt[i];
		bool should_split = 
			ast_stmt_kind(old_s) == STMT_SELECTION &&
			ast_expr_equal(ast_stmt_sel_cond(old_s), cond);
		struct ast_stmt *s = choose_split_path(
			old_s, should_split, enter
		);
		if (!s) {
			continue;
		}
		stmt = realloc(stmt, sizeof(struct ast_stmt *) * ++n);
		stmt[n-1] = ast_stmt_copy(s);
	}
	struct ast_variable **decl = b->decl
		? ast_variables_copy(b->ndecl, b->decl)
		: NULL;
	return ast_block_create(
		decl, b->ndecl,
		stmt, n
	);
}

struct ast_stmt *
choose_split_path(struct ast_stmt *stmt, bool should_split, bool enter)
{
	if (should_split) {
		return enter ? ast_stmt_sel_body(stmt) : NULL;
	}
	return stmt;
}

struct ast_block *
split_block_index(struct ast_block *b, int split_index, bool enter)
{
	int nstmts = ast_block_nstmts(b);
	struct ast_stmt **old_stmt = ast_block_stmts(b);

	int n = 0;
	struct ast_stmt **stmt = NULL;
	for (int i = 0; i < nstmts; i++) {
		struct ast_stmt *s = choose_split_path(
			old_stmt[i], i == split_index, enter
		);
		if (!s) {
			continue;
		}
		stmt = realloc(stmt, sizeof(struct ast_stmt *) * ++n);
		stmt[n-1] = ast_stmt_copy(s);
	}
	struct ast_variable **decl = b->decl
		? ast_variables_copy(b->ndecl, b->decl)
		: NULL;
	return ast_block_create(
		decl, b->ndecl,
		stmt, n
	);
}

static void
func_arr_appendrange(struct func_arr *arr, struct func_arr *range)
{
	for (int i = 0; i < range->n; i++) {
		func_arr_append(arr, range->path[i]);
	}
}

static void
func_arr_append(struct func_arr *arr, struct ast_function *f)
{
	arr->path = realloc(arr->path,
		sizeof(struct ast_function *) * ++arr->n);
	arr->path[arr->n-1] = ast_function_copy(f);
}

void
func_arr_destroy(struct func_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		ast_function_destroy(arr->path[i]);
	}
	free(arr->path);
	free(arr);
}

/* path_verify */

static struct error *
stmt_verify(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state);

void
hack_alloc_p_i_for_iter_verification(struct ast_function *f, struct state *state);

struct error *
path_verify(struct ast_function *f, struct state *state)
{
	struct error *err;

	struct ast_block *body = ast_function_body(f);

	/* declare params and locals in stack frame */
	struct ast_variable **param = ast_function_params(f);
	int nparams = ast_function_nparams(f);
	for (int i = 0; i < nparams; i++) {
		state_declare(state, param[i], true);
	}
	int ndecls = ast_block_ndecls(body);
	struct ast_variable **var = ast_block_decls(body);
	for (int i = 0; i < ndecls; i++) {
		state_declare(state, var[i], false);
	}
	hack_alloc_p_i_for_iter_verification(f, state);

	int nstmts = ast_block_nstmts(body);
	struct ast_stmt **stmt = ast_block_stmts(body);
	for (int i = 0; i < nstmts; i++) {
		struct ast_stmt *s = stmt[i];
		/* TODO: deal with pathing logic so that we only have one path
		 * to terminal points for the function */
		enum ast_stmt_kind kind = ast_stmt_kind(s);
		if (kind == STMT_COMPOUND_V) {
			if ((err = stmt_verify(s, state))) {
				return error_prepend(err, "cannot verify statement: ");
			}
		}
		if ((err = stmt_exec(s, state))) {
			return error_prepend(err, "cannot exec statement: ");
		}
	}
	/* TODO: verify that `result' is of same type as f->result */
	if ((err = abstract_audit(f, state))) {
		return error_prepend(err, "qed error: ");
	}
	return NULL;
}

/* hack_alloc_p_i_for_iter_verification */

static Ref *
hack_ref_from_assertion(struct ast_expr *expr, struct state *state);

static struct error *
alloc_or_unalloc(bool isalloc, Ref *ref, struct state *state);

void
hack_alloc_p_i_for_iter_verification(struct ast_function *f, struct state *state)
{
	struct ast_block *abs = ast_function_abstract(f);
	if (ast_block_nstmts(abs) != 1) {
		return;
	}
	struct ast_stmt *stmt = ast_block_stmts(abs)[0];
	if (ast_stmt_kind(stmt) != STMT_SELECTION) {
		return;
	}
	Ref *ref = hack_ref_from_assertion(
		ast_stmt_sel_cond(stmt), state
	);
	struct error *err = alloc_or_unalloc(true, ref, state);
	assert(!err);
	ref_destroy(ref);
}

/* hack_ref_from_assertion*/

static Ref *
ref_from_expr(struct ast_expr *expr, struct state *state);

static Ref *
hack_ref_from_assertion(struct ast_expr *expr, struct state *state)
{	
	/* get assertand */
	struct ast_expr *assertand = ast_expr_assertion_assertand(expr);

	/* get `assertand' variable */
	return ref_from_expr(assertand, state);
}

static Ref *
ref_from_expr(struct ast_expr *expr, struct state *state)
{
	if (ast_expr_kind(expr) == EXPR_IDENTIFIER) {
		if (strcmp(ast_expr_as_identifier(expr), KEYWORD_RESULT) == 0) {
			return state_getresult(state);
		}
		struct ast_expr *offset = ast_expr_create_constant(0);
		Ref *ref = ref_create_offset(
			ast_expr_as_identifier(expr), 0, offset
		);
		ast_expr_destroy(offset);
		return ref;
	}

	/* otherwise, we must have an access */
	Ref *base = ref_from_expr(ast_expr_access_root(expr), state);
	Ref *ref = ref_create_offset(
		ref_id(base),
		ref_nderef(base) + 1,
		ast_expr_access_index(expr)
	);
	ref_destroy(base);
	return ref;
}



/* stmt_verify */

static struct error *
stmt_v_block_verify(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_expr_verify(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_iter_verify(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_verify(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_NOP:
		return NULL;
	case STMT_COMPOUND_V:
		return stmt_v_block_verify(stmt, state);
	case STMT_EXPR:
		return stmt_expr_verify(stmt, state);
	case STMT_ITERATION:
		return stmt_iter_verify(stmt, state);
	default:
		fprintf(stderr, "cannot verify stmt: %s\n", ast_stmt_str(stmt));
		assert(false);
	}
}

static struct error *
stmt_v_block_verify(struct ast_stmt *v_block_stmt, struct state *state)
{
	struct ast_block *b = ast_stmt_as_v_block(v_block_stmt);
	assert(ast_block_ndecls(b) == 0); /* C89: declarations at beginning of function */
	int nstmts = ast_block_nstmts(b);
	struct ast_stmt **stmt = ast_block_stmts(b);
	for (int i = 0; i < nstmts; i++) {
		struct error *err = stmt_verify(stmt[i], state);
		if (err) {
			return err;
		}
	}
	return NULL;
}

/* stmt_expr_verify */

static bool
expr_decide(struct ast_expr *expr, struct state *state);

static struct error *
stmt_expr_verify(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *expr = ast_stmt_as_expr(stmt);
	if (expr_decide(expr, state)) {
		return NULL;
	}
	return error_create("cannot verify");
}

static bool
expr_unary_decide(struct ast_expr *expr, struct state *state);

static bool
expr_assertion_decide(struct ast_expr *expr, struct state *state);

static bool
expr_decide(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_UNARY:
		return expr_unary_decide(expr, state);
	case EXPR_ASSERTION:
		return expr_assertion_decide(expr, state);
	default:
		assert(false);
	}
}

static bool
expr_unary_decide(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *operand = ast_expr_unary_operand(expr);
	switch (ast_expr_unary_op(expr)) {
	case UNARY_OP_BANG:
		return !expr_decide(operand, state);
	default:
		assert(false);
	}
}

static bool
expr_assertion_decide(struct ast_expr *expr, struct state *state)
{
	Ref *ref = hack_ref_from_assertion(expr, state);
	bool onheap = state_ref_onheap(state, ref);
	ref_destroy(ref);
	return onheap;
}

static struct error *
stmt_iter_verify(struct ast_stmt *stmt, struct state *state)
{
	struct ast_function *f = ast_function_create(
		false,
		ast_type_create(TYPE_VOID, 0),
		dynamic_str("<iter_function>"),
		state_nvariables(state),
		state_getvariables(state),
		ast_block_copy(ast_stmt_iter_abstract(stmt)),
		ast_block_copy(ast_stmt_as_block(ast_stmt_iter_body(stmt)))
	);
	struct error *err = function_verify(f, state_extfunc(state));
	ast_function_destroy(f);
	return err;
}

/* stmt_exec */

typedef struct {
	Heaploc *loc;
	struct error *err;
} Result;

static Result
result_create_empty()
{
	return (Result) { .loc = NULL, .err = NULL };
}

static Result
result_create_error(struct error *err)
{
	return (Result) { .err = err };
}

static bool
result_iserror(Result res)
{
	return res.err;
}

static struct error *
result_error(Result res)
{
	assert(result_iserror(res));
	return res.err;
}

static Result
result_create_heap(Heaploc *loc)
{
	return (Result) { .loc = loc, .err = NULL };
}

static bool
result_onheap(Result res)
{
	assert(!result_iserror(res));
	return res.loc; /* implicit cast */
}

static Heaploc *
result_heaploc(Result res)
{
	assert(!result_iserror(res) && res.loc);
	return res.loc;
}

static struct error *
stmt_compound_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_expr_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_iter_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_jump_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_exec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_COMPOUND:
		return stmt_compound_exec(stmt, state);
	case STMT_COMPOUND_V:
		return NULL;
	case STMT_EXPR:
		return stmt_expr_exec(stmt, state);
	case STMT_ITERATION:
		return stmt_iter_exec(stmt, state);
	case STMT_JUMP:
		return stmt_jump_exec(stmt, state);
	default:
		fprintf(stderr, "stmt->kind: %d\n", stmt->kind);
		assert(false);
	}
}

/* stmt_compound_exec */
static struct error *
stmt_compound_exec(struct ast_stmt *stmt, struct state *state)
{
	struct ast_block *b = stmt->u.compound;
	assert(ast_block_ndecls(b) == 0);
	int nstmt = ast_block_nstmts(b);
	struct ast_stmt **stmts = ast_block_stmts(b);
	for (int i = 0; i < nstmt; i++) {
		struct error *err = stmt_exec(stmts[i], state);
		if (err) {
			return err;
		}
	}
	return NULL;
}

/* stmt_expr_exec */

static Result
expr_exec(struct ast_expr *expr, struct state *state);

static struct error *
stmt_expr_exec(struct ast_stmt *stmt, struct state *state)
{
	Result res = expr_exec(ast_stmt_as_expr(stmt), state);
	if (result_iserror(res)) {
		return result_error(res);
	}
	return NULL;
}

/* expr_exec */

static Result
expr_access_or_identifier_exec(struct ast_expr *expr, struct state *state);

static Result
expr_call_exec(struct ast_expr *expr, struct state *state);

static Result
expr_assign_exec(struct ast_expr *expr, struct state *state);

static Result
expr_exec(struct ast_expr *expr, struct state *state)
{
	/* TODO: verify preconditions of expr (statement) are satisfied */
	/* now add postconditions */
	switch (ast_expr_kind(expr)) {
	case EXPR_CONSTANT:
		return result_create_empty();
	case EXPR_IDENTIFIER:
	case EXPR_ACCESS:
		return expr_access_or_identifier_exec(expr, state);
	case EXPR_CALL:
		return expr_call_exec(expr, state);
	case EXPR_ASSIGNMENT:
		return expr_assign_exec(expr, state);
	default:
		fprintf(stderr, "unknown expr kind `%s'\n", ast_expr_str(expr));
		assert(false);
	}
}

static Result
expr_access_or_identifier_exec(struct ast_expr *expr, struct state *state)
{
	/* XXX: allow for assigning of non-heaploc references to each other */
	Ref *ref = ref_from_expr(expr, state);
	Heaploc *loc = state_ref_get_heaploc(state, ref);
	ref_destroy(ref);
	if (loc) {
		return result_create_heap(loc);
	}
	return result_create_empty();
}

/* expr_call_exec */

struct ast_function *
expr_as_func(struct ast_expr *expr, struct state *state);

static Result
call_exec_inframe(struct ast_expr *expr, struct state *state);

static Result
expr_call_exec(struct ast_expr *expr, struct state *state)
{
	state_pushframe(state, ast_function_type(expr_as_func(expr, state)));
	Result res = call_exec_inframe(expr, state);
	state_popframe(state);
	return res;
}

/* call_type */

struct ast_function *
expr_as_func(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *root = ast_expr_call_root(expr);
	/* TODO: allow function-valued expressions */
	return state_getfunc(state, ast_expr_as_identifier(root));
}

/* call_exec_inframe */

static bool
hack_isfreenull(struct ast_expr *expr);

static struct error *
prepare_parameters(struct ast_function *f, struct ast_expr *expr,
		struct state *state);

static struct ast_expr *
block_reduce(struct ast_block *, struct state *state);

static Result
alloc_or_free(struct ast_expr *red, struct state *state);

static bool
isundefined(struct ast_expr *);

static Result
call_exec_inframe(struct ast_expr *expr, struct state *state)
{
	/* XXX: allow free(NULL) */
	if (hack_isfreenull(expr)) {
		return result_create_empty();
	}

	struct ast_function *f = expr_as_func(expr, state);
	assert(f);
	struct error *err = prepare_parameters(f, expr, state);
	assert(!err);

	/* reduce abstract to proposition we care about */
	struct ast_block *abstract = ast_function_abstract(f);
	struct ast_expr *red_expr = block_reduce(abstract, state);
	if (!red_expr) {
		return result_create_empty();
	}

	/* undefined */
	if (isundefined(red_expr)) {
		return result_create_error(error_create("undefined behaviour"));
	}

	/* alloc or free */
	Result res = alloc_or_free(red_expr, state);
	ast_expr_destroy(red_expr);
	return res;
}

static bool
hack_isfreenull(struct ast_expr *expr)
{
	/* check root is "free" */
	struct ast_expr *root = ast_expr_call_root(expr);
	char *id = ast_expr_as_identifier(root);
	if (strcmp(id, "free") != 0) {
		return false;
	}
	assert(ast_expr_call_nargs(expr) == 1);

	struct ast_expr *arg = ast_expr_call_args(expr)[0];
	return ast_expr_kind(arg) == EXPR_IDENTIFIER
		&& strcmp(ast_expr_as_identifier(arg), "NULL") == 0;
}

static bool
isundefined(struct ast_expr *expr)
{
	return ast_expr_memory_isundefined(expr);
}

/* prepare_parameters: Allocate arguments in call expression and assign them to
 * their respective parameters. */
static struct error *
prepare_parameters(struct ast_function *f, struct ast_expr *expr,
		struct state *state)
{
	int nargs = ast_expr_call_nargs(expr);
	struct ast_expr **arg = ast_expr_call_args(expr);

	struct ast_variable **param = ast_function_params(f);

	assert(ast_function_nparams(f) == nargs);

	for (int i = 0; i < nargs; i++) {
		/* in the case of exprs that aren't calls we want to eval */
		state_declare(state, param[i], true);
		Result res = expr_exec(arg[i], state);
		if (result_onheap(res)) {
			char *param_name = ast_variable_name(param[i]);

			struct ast_expr *offset = ast_expr_create_constant(0);
			Ref *ref = ref_create_offset(
				param_name, 0, offset
			);
			ast_expr_destroy(offset);

			state_ref_assign_heaploc(
				state, ref, result_heaploc(res)
			);

			ref_destroy(ref);
		}
	}
	return NULL;
}

/* block_reduce */

static struct ast_expr *
stmt_reduce(struct ast_stmt *stmt, struct state *state);

static struct ast_expr *
block_reduce(struct ast_block *b, struct state *state)
{
	assert(ast_block_nstmts(b) == 1); /* XXX */
	struct ast_stmt **stmt = ast_block_stmts(b);
	return stmt_reduce(stmt[0], state);
}

/* stmt_reduce */

static struct ast_expr *
stmt_expr_reduce(struct ast_stmt *stmt, struct state *state);

static struct ast_expr *
stmt_sel_reduce(struct ast_stmt *stmt, struct state *state);

static struct ast_expr *
stmt_reduce(struct ast_stmt *stmt, struct state *state)
{
	switch (stmt->kind) {
	case STMT_EXPR:
		return stmt_expr_reduce(stmt, state);
	case STMT_SELECTION:
		return stmt_sel_reduce(stmt, state);
	default:
		assert(false);
	}
}

static struct ast_expr *
stmt_expr_reduce(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *expr = ast_stmt_as_expr(stmt);
	/* XXX: only dealing with allocs */
	assert(ast_expr_kind(expr) == EXPR_MEMORY);
	return ast_expr_copy(expr);
}

/* stmt_sel_reduce */

static struct ast_expr *
hack_force_sel_body_as_alloc_or_identifier(struct ast_stmt *body);

static struct ast_expr *
stmt_sel_reduce(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *cond = ast_stmt_sel_cond(stmt);
	if (!expr_decide(cond, state)) {
		struct ast_stmt *nest = ast_stmt_sel_nest(stmt);
		if (nest) {
			return stmt_reduce(nest, state);
		} 
		return NULL;
	}
	/* XXX: only dealing with allocs or identifiers (for `undefined') */
	struct ast_stmt *body = ast_stmt_sel_body(stmt);
	return hack_force_sel_body_as_alloc_or_identifier(body);
}

static struct ast_expr *
hack_force_sel_body_as_alloc_or_identifier(struct ast_stmt *body)
{
	struct ast_expr *alloc = ast_stmt_as_expr(body);
	enum ast_expr_kind kind = ast_expr_kind(alloc);
	assert(kind == EXPR_MEMORY || kind == EXPR_IDENTIFIER);
	return ast_expr_copy(alloc);
}

/* alloc_or_free: Create or free Heaploc in state as dictated by the given
 * expression `alloc'. */
static Result
alloc_or_free(struct ast_expr *mem, struct state *state)
{
	struct ast_expr *root = ast_expr_memory_root(mem);
	if (ast_expr_memory_isalloc(mem)) {
		assert(strcmp(ast_expr_as_identifier(root), KEYWORD_RESULT) == 0);
		Heaploc *loc = state_alloc(state);
		return result_create_heap(loc);
	}

	Ref *ref = ref_from_expr(root, state);
	Heaploc *loc = state_ref_get_heaploc(state, ref);
	ref_destroy(ref);

	assert(loc); /* XXX: probably user error */
	struct error *err = state_heaploc_free(state, loc);
	if (err) {
		fprintf(stderr, "cannot free: %s\n", err->msg);
		assert(false);
	}
	return result_create_empty();
}

static Result
expr_assign_exec(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *lval = ast_expr_assignment_lval(expr),
			*rval = ast_expr_assignment_rval(expr);

	Result res = expr_exec(rval, state);
	if (result_onheap(res)) {
		Ref *ref = ref_from_expr(lval, state);

		state_ref_assign_heaploc(state, ref, result_heaploc(res));

		ref_destroy(ref);
	}
	return res;
}

static struct ast_expr *
iter_abstract_action(struct ast_stmt *iter, struct state *state);

static struct pointer_deref { char *id; int nderef; }
pointer_deref_from_expr(struct ast_expr *expr);

static struct error *
alloc_or_unalloc(bool isalloc, Ref *ref, struct state *state);

static struct error *
stmt_iter_exec(struct ast_stmt *stmt, struct state *state)
{
	struct error *err;

	/* check internal consistency of iteration */
	if ((err = stmt_verify(stmt, state))) {
		return err;
	}

	struct ast_expr *action = iter_abstract_action(stmt, state);
	if (!action) {
		return NULL;
	}
	if (isundefined(action)) {
		return error_create("undefined");
	}

	/* our assumption going forward is that action is a memory expression */
	struct ast_expr *mem = action;

	/* get ref to object allocated/freed */
	struct pointer_deref pd = pointer_deref_from_expr(
		ast_expr_memory_root(mem)
	);
	Ref *ref = ref_create_range(
		pd.id,
		pd.nderef,
		ast_stmt_iter_lower_bound(stmt),
		ast_stmt_iter_upper_bound(stmt)
	);

	err = alloc_or_unalloc(
		ast_expr_memory_isalloc(mem), ref, state
	);

	ref_destroy(ref);

	return err;
}

/* iter_abstract_action */

static struct ast_expr *
stmt_itereval(struct ast_stmt *stmt, struct ast_stmt *iter, struct state *state);

static struct ast_expr *
iter_abstract_action(struct ast_stmt *iter, struct state *state)
{
	struct ast_block *abs = ast_stmt_iter_abstract(iter);
	assert(abs);

	int nstmts = ast_block_nstmts(abs);
	if (nstmts == 0) {
		return NULL;
	}

	assert(ast_block_ndecls(abs) == 0 && nstmts == 1);
	struct ast_stmt **stmt = ast_block_stmts(abs);
	assert(!ast_block_decls(abs) && stmt);
	return stmt_itereval(stmt[0], iter, state);
}

/* stmt_itereval */

static struct ast_expr *
stmt_expr_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state);

static struct ast_expr *
stmt_compound_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state);

static struct ast_expr *
stmt_sel_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state);

static struct ast_expr *
stmt_itereval(struct ast_stmt *stmt, struct ast_stmt *iter, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_EXPR:
		return stmt_expr_itereval(stmt, iter, state);
	case STMT_COMPOUND:
		return stmt_compound_itereval(stmt, iter, state);
	case STMT_SELECTION:
		return stmt_sel_itereval(stmt, iter, state);
	default:
		assert(false);
	}
}

static struct ast_expr *
stmt_expr_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_expr *expr = ast_stmt_as_expr(stmt);
	assert(ast_expr_kind(expr) == EXPR_MEMORY || isundefined(expr));
	return expr;
}

static struct ast_expr *
stmt_compound_itereval(struct ast_stmt *compound, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_block *b = ast_stmt_as_block(compound);
	assert(ast_block_ndecls(b) == 0 && ast_block_nstmts(b) == 1);
	struct ast_stmt **stmt = ast_block_stmts(b);
	assert(!ast_block_decls(b) && stmt);
	return stmt_itereval(stmt[0], iter, state);
}


/* stmt_sel_itereval */

static bool
expr_iter_decide(struct ast_expr *, struct ast_stmt *iter, struct state *state);

static struct ast_expr *
stmt_sel_itereval(struct ast_stmt *sel, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_expr *cond = ast_stmt_sel_cond(sel);
	if (expr_iter_decide(cond, iter, state)) {
		return stmt_itereval(ast_stmt_sel_body(sel), iter, state);
	}

	struct ast_stmt *nest = ast_stmt_sel_nest(sel);
	assert(nest);
	return stmt_itereval(nest, iter, state);
}

static bool
expr_unary_iter_decide(struct ast_expr *expr, struct ast_stmt *iter,
		struct state *state);

static bool
expr_assertion_iter_decide(struct ast_expr *expr, struct ast_stmt *iter,
		struct state *state);

static bool
expr_iter_decide(struct ast_expr *expr, struct ast_stmt *iter,
		struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_UNARY:
		return expr_unary_iter_decide(expr, iter, state);
	case EXPR_ASSERTION:
		return expr_assertion_iter_decide(expr, iter, state);
	default:
		assert(false);
	}
}

static bool
expr_unary_iter_decide(struct ast_expr *expr, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_expr *operand = ast_expr_unary_operand(expr);
	switch (ast_expr_unary_op(expr)) {
	case UNARY_OP_BANG:
		return !expr_iter_decide(operand, iter, state);
	default:
		assert(false);
	}
}

static struct ast_expr *
hack_access_from_assertion(struct ast_expr *expr);

static bool
expr_assertion_iter_decide(struct ast_expr *expr, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_expr *access = hack_access_from_assertion(expr);

	struct pointer_deref pd = pointer_deref_from_expr(access);

	Ref *ref = ref_create_range(
		pd.id,
		pd.nderef,
		ast_stmt_iter_lower_bound(iter),
		ast_stmt_iter_upper_bound(iter)
	);

	bool onheap = state_ref_onheap(state, ref);

	ref_destroy(ref);

	return onheap;
}

static struct ast_expr *
hack_access_from_assertion(struct ast_expr *expr)
{
	struct ast_expr *assertand = ast_expr_assertion_assertand(expr);
	assert(ast_expr_kind(assertand) == EXPR_ACCESS);
	return assertand;
}


static struct pointer_deref
pointer_deref_from_expr(struct ast_expr *expr)
{
	if (ast_expr_kind(expr) == EXPR_IDENTIFIER) {
		return (struct pointer_deref) {
			.id	= ast_expr_as_identifier(expr),
			.nderef	= 0,
		};
	}

	/* otherwise, we must have an access */
	struct pointer_deref pd =
		pointer_deref_from_expr(ast_expr_access_root(expr));
	return (struct pointer_deref) {
		.id	= pd.id,
		.nderef	= pd.nderef + 1,
	};
}

static struct error *
alloc_or_unalloc(bool isalloc, Ref *ref, struct state *state)
{
	if (isalloc) {
		state_ref_assign_heaploc(state, ref, state_alloc(state));
		return NULL;
	}
	return state_ref_unalloc(state, ref);
}

static struct error *
stmt_jump_exec(struct ast_stmt *stmt, struct state *state)
{
	Result res = expr_exec(ast_stmt_jump_rv(stmt), state);
	if (result_onheap(res)) {
		state_result_assign(state, result_heaploc(res));
	}
	return NULL; /* TODO: return error from above */
}

/* abstract_audit */

struct ast_block *
hack_flatten_abstract_for_iter_verification(struct ast_function *f,
		struct state* state);

static struct error *
function_absexec(struct ast_block *abs, struct state *initial_state);

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state)
{
	struct error *err;

	if (!state_heap_referenced(actual_state)) {
		return error_create("unaddressed memory on heap");
	}

	struct state *alleged_state = state_create(
		state_extfunc(actual_state), ast_function_type(f)
	);

	struct ast_block *body = ast_function_body(f);

	/* declare params and locals in stack frame */
	struct ast_variable **param = ast_function_params(f);
	int nparams = ast_function_nparams(f);
	for (int i = 0; i < nparams; i++) {
		state_declare(alleged_state, param[i], true);
	}
	int ndecls = ast_block_ndecls(body);
	struct ast_variable **var = ast_block_decls(body);
	for (int i = 0; i < ndecls; i++) {
		state_declare(alleged_state, var[i], false);
	}
	hack_alloc_p_i_for_iter_verification(f, alleged_state);

	struct ast_block *abs = hack_flatten_abstract_for_iter_verification(
		f, actual_state
	);

	/* mutates alleged_state */
	if ((err = function_absexec(abs, alleged_state))) {
		return err;
	}

	ast_block_destroy(abs);

	bool equiv = state_abstractly_equivalent(actual_state, alleged_state);

	state_destroy(alleged_state); /* actual_state handled by caller */ 
	
	if (!equiv) {
		/* XXX: print states */
		return error_create("actual and alleged states differ");
	}

	return NULL;
}

struct ast_block *
hack_flatten_abstract_for_iter_verification(struct ast_function *f,
		struct state *state)
{
	struct ast_block *abs = ast_function_abstract(f);
	if (ast_block_nstmts(abs) != 1) {
		return ast_block_copy(abs);
	}
	struct ast_stmt *stmt = ast_block_stmts(abs)[0];
	if (ast_stmt_kind(stmt) != STMT_SELECTION) {
		return ast_block_copy(abs);
	}
	/* asserts that we have `allocated' condition */
	ref_destroy(
		hack_ref_from_assertion(
			ast_stmt_sel_cond(stmt), state
		)
	);
	return ast_block_copy(ast_stmt_as_block(ast_stmt_sel_body(stmt)));
}

/* function_absexec */

static struct error *
stmt_absexec(struct ast_stmt *stmt, struct state *state);

static struct error *
function_absexec(struct ast_block *abs, struct state *state)
{
	int nstmts = ast_block_nstmts(abs);
	struct ast_stmt **stmt = ast_block_stmts(abs);
	for (int i = 0; i < nstmts; i++) {
		struct error *err;
		if ((err = stmt_absexec(stmt[i], state))) {
			return err;
		}
	}
	assert(state_heap_referenced(state));
	return NULL;
}

/* stmt_absexec */

static struct error *
expr_absexec(struct ast_expr *expr, struct state *state);

static struct error *
sel_absexec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_absexec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_EXPR:
		return expr_absexec(ast_stmt_as_expr(stmt), state);
	case STMT_SELECTION:
		return sel_absexec(stmt, state);
	default:
		assert(false);
	}
}

/* expr_absexec */

static struct error *
mem_absexec(struct ast_expr *mem, struct state *state);

static struct error *
expr_absexec(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_IDENTIFIER:
		assert(isundefined(expr));
		return error_create("undefined");
	case EXPR_MEMORY:
		return mem_absexec(expr, state);
	default:
		assert(false);
	}
}

static struct error *
mem_absexec(struct ast_expr *mem, struct state *state)
{
	Ref *ref = ref_from_expr(ast_expr_memory_root(mem), state);

	struct error *err = alloc_or_unalloc(
		ast_expr_memory_isalloc(mem), ref, state
	);
	ref_destroy(ref);
	return err;
}

static struct error *
sel_absexec(struct ast_stmt *stmt, struct state *state)
{
	assert(false);
}
