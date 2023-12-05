#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ext.h"
#include "ast.h"
#include "object.h"
#include "state.h"
#include "util.h"
#include "value.h"
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
function_verify_paths(Funcarr paths, struct externals *);

struct error *
function_verify(struct ast_function *f, struct externals *ext)
{
	Funcarr paths = paths_fromfunction(f);
	struct error *err = function_verify_paths(paths, ext);
	func_arr_destroy(paths);
	return err;
}

struct error *
path_verify_withstate(struct ast_function *, struct externals *);

struct error *
function_verify_paths(Funcarr paths, struct externals *ext)
{	
	int len = func_arr_len(paths);
	struct ast_function **path = func_arr_paths(paths);
	for (int i = 0; i < len; i++) {
		struct error *err = NULL;
		if ((err = path_verify_withstate(path[i], ext))) {
			return err;
		}
	}
	return NULL;
}

/* path_verify_withstate */

struct error *
path_verify(struct ast_function *f, struct state *state, struct externals *);

struct error *
path_verify_withstate(struct ast_function *f, struct externals *ext)
{
	struct state *state = state_create(ext, ast_function_type(f));
	struct error *err = path_verify(f, state, ext);
	state_destroy(state);
	return err;
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
abstract_audit(struct ast_function *f, struct state *actual_state,
		struct externals *);

static struct error *
parameterise_state(struct state *s, struct ast_function *f);

struct error *
path_verify(struct ast_function *f, struct state *state, struct externals *ext)
{
	struct error *err = NULL;

	struct ast_block *body = ast_function_body(f);

	if ((err = parameterise_state(state, f))) {
		return err;
	}

	int ndecls = ast_block_ndecls(body);
	struct ast_variable **var = ast_block_decls(body);
	for (int i = 0; i < ndecls; i++) {
		state_declare(state, var[i], false);
	}

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
	state_undeclarevars(state);
	/* TODO: verify that `result' is of same type as f->result */
	if ((err = abstract_audit(f, state, ext))) {
		return error_prepend(err, "qed error: ");
	}
	return NULL;
}

static bool
isprecondition(struct ast_stmt *);

static struct error *
parameterise_state(struct state *s, struct ast_function *f)
{
	/* declare params and locals in stack frame */
	struct ast_variable **param = ast_function_params(f);
	int nparams = ast_function_nparams(f);
	for (int i = 0; i < nparams; i++) {
		struct ast_variable *p = param[i];
		state_declare(s, p, true);
		if (ast_type_base(ast_variable_type(p)) == TYPE_INT) {
			struct object *obj = state_getobject(s, ast_variable_name(p));
			assert(obj);
			object_assign(obj, state_vconst(s));
		}
	}

	struct ast_block *abs = ast_function_abstract(f);
	int nstmts = ast_block_nstmts(abs);
	struct ast_stmt **stmt = ast_block_stmts(abs);
	for (int i = 0; i < nstmts; i++) {
		struct error *err = NULL;
		if (isprecondition(stmt[i])) {
			if ((err = stmt_exec(stmt[i], s))) {
				return err;
			}
		}
	}

	return NULL;
}

static bool
isprecondition(struct ast_stmt *stmt)
{
	return ast_stmt_kind(stmt) == STMT_LABELLED
		&& strcmp(ast_stmt_labelled_label(stmt), "pre") == 0;
}


struct lvalue;

struct lvalue *
lvalue_create(struct ast_type *, struct object *);

void
lvalue_destroy(struct lvalue *);

struct ast_type *
lvalue_type(struct lvalue *);

struct object *
lvalue_object(struct lvalue *);

struct lvalue *
expr_lvalue(struct ast_expr *, struct state *);

static struct object *
hack_object_from_assertion(struct ast_expr *expr, struct state *state)
{	
	/* get assertand */
	struct ast_expr *assertand = ast_expr_assertion_assertand(expr);

	/* get `assertand' variable */
	struct object *obj = lvalue_object(expr_lvalue(assertand, state));
	assert(obj);
	return obj;
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

struct result {
	struct value *val;
	struct error *err;
};

static struct result
result_error_create(struct error *err)
{
	assert(err);
	return (struct result) { .err = err };
}

static struct result
result_value_create(struct value *val)
{
	return (struct result) { .val = val, .err = NULL };
}

static void
result_destroy(struct result res)
{
	assert(!res.err);
	if (res.val) {
		value_destroy(res.val);
	}
}

static bool
result_iserror(struct result res)
{
	return res.err;
}

static struct error *
result_as_error(struct result res)
{
	assert(res.err);
	return res.err;
}

static struct value *
result_as_value(struct result res)
{
	assert(!res.err && res.val);
	return res.val;
}

static bool
result_hasvalue(struct result res)
{
	assert(!result_iserror(res));
	return res.val; /* implicit cast */
}

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
expr_binary_decide(struct ast_expr *expr, struct state *state);

static bool
expr_decide(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_UNARY:
		return expr_unary_decide(expr, state);
	case EXPR_ASSERTION:
		return expr_assertion_decide(expr, state);
	case EXPR_BINARY:
		return expr_binary_decide(expr, state);
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
	struct object *obj = hack_object_from_assertion(expr, state);
	bool isdeallocand = state_addresses_deallocand(state, obj);
	return isdeallocand;
}

static struct result
expr_eval(struct ast_expr *expr, struct state *state);

static bool
expr_binary_decide(struct ast_expr *expr, struct state *state)
{
	struct result root = expr_eval(ast_expr_binary_e1(expr), state),
	       last = expr_eval(ast_expr_binary_e2(expr), state);

	assert(!result_iserror(root) && !result_iserror(last));

	return value_compare(
		result_as_value(root),
		ast_expr_binary_op(expr),
		result_as_value(last)
	);
}

static bool
iter_empty(struct ast_stmt *stmt, struct state *state);

static bool
expr_iter_verify(struct ast_expr *expr, struct ast_expr *up,
		struct ast_expr *lw, struct state *state);

static struct error *
stmt_iter_verify(struct ast_stmt *stmt, struct state *state)
{
	/* check for empty sets */
	if (iter_empty(stmt, state)) {
		return NULL;
	}

	/* neteffect is an iter statement */
	struct ast_stmt *body = ast_stmt_iter_body(stmt);
	assert(ast_stmt_kind(body) == STMT_COMPOUND);
	struct ast_block *block = ast_stmt_as_block(body);
	assert(ast_block_ndecls(block) == 0 && ast_block_nstmts(block) == 1);
	struct ast_expr *assertion = ast_stmt_as_expr(ast_block_stmts(block)[0]);

	/* we're currently discarding analysis of `offset` and relying on the
	 * bounds (lw, up beneath) alone */
	struct ast_expr *lw = ast_stmt_iter_lower_bound(stmt),
			*up = ast_stmt_iter_upper_bound(stmt);

	if (!expr_iter_verify(assertion, lw, up, state)) {
		return error_create("could not verify");	
	}
	return NULL;
}

static bool
iter_empty(struct ast_stmt *stmt, struct state *state)
{
	struct error *err = stmt_exec(ast_stmt_iter_init(stmt), state);
	assert(!err);
	/* iter is empty if its cond is false after init (executed above) */
	return !expr_decide(ast_stmt_as_expr(ast_stmt_iter_cond(stmt)), state);
}

static bool
expr_iter_unary_verify(struct ast_expr *expr, struct ast_expr *lw,
		struct ast_expr *up, struct state *);

static bool
expr_iter_assertion_verify(struct ast_expr *expr, struct ast_expr *lw,
		struct ast_expr *up, struct state *);

static bool
expr_iter_verify(struct ast_expr *expr, struct ast_expr *lw,
		struct ast_expr *up, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_UNARY:
		return expr_iter_unary_verify(expr, lw, up, state);
	case EXPR_ASSERTION:
		return expr_iter_assertion_verify(expr, lw, up, state);
	default:
		assert(false);
	}
}

static bool
expr_iter_unary_verify(struct ast_expr *expr, struct ast_expr *lw,
		struct ast_expr *up, struct state *state)
{
	struct ast_expr *operand = ast_expr_unary_operand(expr);
	switch (ast_expr_unary_op(expr)) {
	case UNARY_OP_BANG:
		return !expr_iter_verify(operand, lw, up, state);
	default:
		assert(false);
	}
}

static bool
expr_iter_assertion_verify(struct ast_expr *expr, struct ast_expr *lw,
		struct ast_expr *up, struct state *state)
{
	struct ast_expr *acc = ast_expr_assertion_assertand(expr); /* `*(arr+offset)` */
	assert(ast_expr_unary_op(acc) == UNARY_OP_DEREFERENCE);
	struct ast_expr *inner = ast_expr_unary_operand(acc); /* `arr+offset` */
	struct ast_expr *i = ast_expr_identifier_create(dynamic_str("i"));
	struct ast_expr *j = ast_expr_identifier_create(dynamic_str("j"));
	assert(
		ast_expr_equal(ast_expr_binary_e2(inner), i) ||
		ast_expr_equal(ast_expr_binary_e2(inner), j)
	);
	ast_expr_destroy(j);
	ast_expr_destroy(i);
	struct object *obj = lvalue_object(
		expr_lvalue(ast_expr_binary_e1(acc), state)
	);
	assert(obj);

	bool deallocands = state_range_aredeallocands(
		state, obj, lw, up	
	);

	return deallocands;
}


/* stmt_exec */

static struct error *
stmt_compound_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
expr_exec(struct ast_expr *exec, struct state *state);

static struct error *
stmt_iter_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_jump_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_exec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_LABELLED:
		return stmt_exec(ast_stmt_labelled_stmt(stmt), state);
	case STMT_COMPOUND:
		return stmt_compound_exec(stmt, state);
	case STMT_COMPOUND_V:
		return NULL;
	case STMT_EXPR:
		return expr_exec(ast_stmt_as_expr(stmt), state);
	case STMT_ITERATION:
		return stmt_iter_exec(stmt, state);
	case STMT_JUMP:
		return stmt_jump_exec(stmt, state);
	default:
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

/* expr_lvalue */

struct lvalue *
expr_identifier_lvalue(struct ast_expr *expr, struct state *state);

struct lvalue *
expr_access_lvalue(struct ast_expr *expr, struct state *state);

struct lvalue *
expr_unary_lvalue(struct ast_expr *expr, struct state *state);

struct lvalue *
expr_structmember_lvalue(struct ast_expr *expr, struct state *state);

struct lvalue *
expr_lvalue(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_IDENTIFIER:
		return expr_identifier_lvalue(expr, state);
	case EXPR_UNARY:
		return expr_unary_lvalue(expr, state);
	case EXPR_STRUCTMEMBER:
		return expr_structmember_lvalue(expr, state);
	default:
		assert(false);
	}
}

struct lvalue *
expr_identifier_lvalue(struct ast_expr *expr, struct state *state)
{
	char *id = ast_expr_as_identifier(expr);
	return lvalue_create(
		state_getobjecttype(state, id),
		state_getobject(state, id)
	);
}

struct lvalue *
expr_unary_lvalue(struct ast_expr *expr, struct state *state)
{
	assert(ast_expr_unary_op(expr) == UNARY_OP_DEREFERENCE);
	struct ast_expr *inner = ast_expr_unary_operand(expr);

	struct lvalue *root = expr_lvalue(ast_expr_binary_e1(inner), state);
	struct object *root_obj = lvalue_object(root);
	if (!root_obj) { /* `root` freed */
		return NULL;
	}
	struct ast_type *t = ast_type_ptr_type(lvalue_type(root));

	struct value *root_val = object_as_value(root_obj);
	assert(root_val);
	struct object *obj = state_deref(
		state, root_val, ast_expr_binary_e2(inner)
	);

	return lvalue_create(t, obj);
}


struct lvalue *
expr_structmember_lvalue(struct ast_expr *expr, struct state *state)
{
	struct lvalue *root = expr_lvalue(ast_expr_member_root(expr), state);
	struct object *root_obj = lvalue_object(root);
	assert(root_obj);
	struct object *obj = object_getmember(
		root_obj,
		lvalue_type(root),
		ast_expr_member_field(expr),
		state
	);
	struct ast_type *t = object_getmembertype(
		root_obj,
		lvalue_type(root),
		ast_expr_member_field(expr),
		state
	);
	return lvalue_create(t, obj);
}


struct lvalue {
	struct ast_type *t;
	struct object *obj;
};

struct lvalue *
lvalue_create(struct ast_type *t, struct object *obj)
{
	struct lvalue *l = malloc(sizeof(struct lvalue));
	l->t = t;
	l->obj = obj;
	return l;
}

void
lvalue_destroy(struct lvalue *l)
{
	ast_type_destroy(l->t);
	object_destroy(l->obj);
	free(l);
}

struct ast_type *
lvalue_type(struct lvalue *l)
{
	return l->t;
}

struct object *
lvalue_object(struct lvalue *l)
{
	return l->obj;
}





/* stmt_expr_eval */

static struct error *
expr_exec(struct ast_expr *expr, struct state *state)
{
	struct result res = expr_eval(expr, state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}
	result_destroy(res);
	return NULL;
}

/* expr_eval */

static struct result
expr_constant_eval(struct ast_expr *expr, struct state *state);

static struct result
expr_literal_eval(struct ast_expr *expr, struct state *state);

static struct result
expr_identifier_eval(struct ast_expr *expr, struct state *state);

static struct result
expr_unary_eval(struct ast_expr *expr, struct state *state);

static struct result
expr_structmember_eval(struct ast_expr *expr, struct state *state);

static struct result
expr_call_eval(struct ast_expr *expr, struct state *state);

static struct result
expr_assign_eval(struct ast_expr *expr, struct state *state);

static struct result
expr_incdec_eval(struct ast_expr *expr, struct state *state);

static struct result
expr_binary_eval(struct ast_expr *expr, struct state *state);

static struct result
expr_eval(struct ast_expr *expr, struct state *state)
{
	/* TODO: verify preconditions of expr (statement) are satisfied */
	/* now add postconditions */
	switch (ast_expr_kind(expr)) {
	case EXPR_CONSTANT:
		return expr_constant_eval(expr, state);
	case EXPR_STRING_LITERAL:
		return expr_literal_eval(expr, state);
	case EXPR_IDENTIFIER:
		return expr_identifier_eval(expr, state);
	case EXPR_UNARY:
		return expr_unary_eval(expr, state);
	case EXPR_STRUCTMEMBER:
		return expr_structmember_eval(expr, state);
	case EXPR_CALL:
		return expr_call_eval(expr, state);
	case EXPR_ASSIGNMENT:
		return expr_assign_eval(expr, state);
	case EXPR_INCDEC:
		return expr_incdec_eval(expr, state);
	case EXPR_BINARY:
		return expr_binary_eval(expr, state);
	default:
		assert(false);
	}
}

static struct result
expr_literal_eval(struct ast_expr *expr, struct state *state)
{
	return result_value_create(
		value_literal_create(ast_expr_as_literal(expr))
	);
}

static struct result
expr_constant_eval(struct ast_expr *expr, struct state *state)
{
	return result_value_create(
		value_int_create(ast_expr_as_constant(expr))
	);
}

static struct result
expr_identifier_eval(struct ast_expr *expr, struct state *state)
{
	struct object *obj = state_getobject(state, ast_expr_as_identifier(expr));
	if (!obj) {
		return result_error_create(error_create("no object"));
	}
	struct value *val = object_as_value(obj);
	if (!val) {
		return result_error_create(error_create("no value"));
	}
	return result_value_create(value_copy(val));
}

static struct result
expr_unary_eval(struct ast_expr *expr, struct state *state)
{
	assert(ast_expr_unary_op(expr) == UNARY_OP_DEREFERENCE);

	struct ast_expr *inner = ast_expr_unary_operand(expr); /* arr+offset */

	struct result res = expr_eval(ast_expr_binary_e1(inner), state);
	if (result_iserror(res)) {
		return res;
	}
	struct value *arr = result_as_value(res);
	assert(arr);
	struct object *obj = state_deref(state, arr, ast_expr_binary_e2(inner));
	assert(obj);
	result_destroy(res);

	struct value *v = object_as_value(obj);
	assert(v);

	return result_value_create(value_copy(v));
}

static struct result
expr_structmember_eval(struct ast_expr *expr, struct state *s)
{
	struct result res = expr_eval(ast_expr_member_root(expr), s);
	if (result_iserror(res)) {
		return res;
	}
	struct value *v = value_copy(object_as_value(
		value_struct_member(
			result_as_value(res),
			ast_expr_member_field(expr)
		)
	));
	result_destroy(res);
	return result_value_create(v);
}

/* expr_call_eval */

struct ast_function *
expr_as_func(struct ast_expr *expr, struct state *state);

struct result_arr {
	int n;
	struct result *res;
};

struct result_arr *
result_arr_create()
{
	return calloc(1, sizeof(struct result_arr));
}

void
result_arr_destroy(struct result_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		result_destroy(arr->res[i]);
	}
	free(arr);
}

void
result_arr_append(struct result_arr *arr, struct result res)
{
	arr->res = realloc(arr->res, sizeof(struct result) * ++arr->n);
	arr->res[arr->n-1] = res;
}

static struct result
prepare_argument(struct ast_expr *arg, struct ast_variable *param, struct state *);

struct result_arr *
prepare_arguments(struct ast_expr *call, struct state *state)
{
	struct result_arr *args = result_arr_create();

	int nargs = ast_expr_call_nargs(call);
	struct ast_expr **arg = ast_expr_call_args(call);

	struct ast_function *f = expr_as_func(call, state);
	int nparams = ast_function_nparams(f);
	struct ast_variable **param = ast_function_params(f);

	assert(nargs == nparams);

	for (int i = 0; i < nargs; i++) {
		result_arr_append(
			args, prepare_argument(arg[i], param[i], state)
		);
	}

	return args;
}

static struct result
prepare_argument(struct ast_expr *arg, struct ast_variable *param, struct state *s)
{
	if (ast_expr_kind(arg) != EXPR_ARBARG) {
		return expr_eval(arg, s);
	}
	assert(ast_type_base(ast_variable_type(param)) == TYPE_INT);
	return result_value_create(state_vconst(s));
}


static struct result
call_eval_inframe(struct ast_expr *expr, struct state *state, struct result_arr *args);

static struct result
expr_call_eval(struct ast_expr *expr, struct state *state)
{
	struct result_arr *args = prepare_arguments(expr, state);
	state_pushframe(state, ast_function_type(expr_as_func(expr, state)));
	struct result res = call_eval_inframe(expr, state, args);
	result_arr_destroy(args);
	if (result_iserror(res)) {
		return res;
	}
	if (result_hasvalue(res)) { /* preserve value through pop */
		res = result_value_create(value_copy(result_as_value(res)));
	}
	state_popframe(state);
	return res;
}

/* call_type */

struct ast_function *
expr_as_func(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *root = ast_expr_call_root(expr);
	/* TODO: allow function-valued expressions */
	return externals_getfunc(state_getext(state), ast_expr_as_identifier(root));
}

/* call_eval_inframe */

static struct error *
prepare_parameters(struct ast_function *f, struct result_arr *args,
		struct state *state);

static struct result
function_absexec(struct ast_block *abs, struct state *state);

static struct result
call_eval_inframe(struct ast_expr *expr, struct state *state, struct result_arr *args)
{
	struct ast_function *f = expr_as_func(expr, state);
	assert(f);

	struct error *err = prepare_parameters(f, args, state);
	assert(!err);

	return function_absexec(ast_function_abstract(f), state);
}

/* prepare_parameters: Allocate arguments in call expression and assign them to
 * their respective parameters. */
static struct error *
prepare_parameters(struct ast_function *f, struct result_arr *args,
		struct state *state)
{
	struct ast_variable **param = ast_function_params(f);

	assert(ast_function_nparams(f) == args->n);

	for (int i = 0; i < args->n; i++) {
		state_declare(state, param[i], true);

		struct result res = args->res[i];
		if (result_iserror(res)) {
			return result_as_error(res);
		}

		if (!result_hasvalue(res)) {
			continue;
		}

		struct ast_expr *name = ast_expr_identifier_create(
			dynamic_str(ast_variable_name(param[i]))
		);
		struct object *obj = lvalue_object(expr_lvalue(name, state));
		ast_expr_destroy(name);

		object_assign(obj, value_copy(result_as_value(res)));
	}
	return NULL;
}

static struct result
expr_assign_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *lval = ast_expr_assignment_lval(expr),
			*rval = ast_expr_assignment_rval(expr);

	struct result res = expr_eval(rval, state);
	if (result_hasvalue(res)) {
		struct object *obj = lvalue_object(expr_lvalue(lval, state));
		assert(obj);
		object_assign(obj, value_copy(result_as_value(res)));
	}
	return res;
}

static struct result
expr_incdec_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *assign = ast_expr_incdec_to_assignment(expr);

	struct result res;

	if (ast_expr_incdec_pre(expr)) { /* ++i */
		res = expr_assign_eval(assign, state);
	} else { /* i++ */
		res = expr_eval(ast_expr_incdec_root(expr), state);
		/* assign and ignore result */ 
		result_destroy(expr_assign_eval(assign, state));
	}

	ast_expr_destroy(assign);

	return res;
}

static struct result
expr_binary_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *e1 = ast_expr_binary_e1(expr),
			*e2 = ast_expr_binary_e2(expr);
	struct result res1 = expr_eval(e1, state),
	       res2 = expr_eval(e2, state);
	if (result_iserror(res1)) {
		return res1;
	}
	if (result_iserror(res2)) {
		return res2;
	}
	return result_value_create(
		value_int_sync_create(
			ast_expr_binary_create(
				value_to_expr(result_as_value(res1)),
				ast_expr_binary_op(expr),
				value_to_expr(result_as_value(res2))
			)
		)
	);
}


static struct ast_stmt *
iter_neteffect(struct ast_stmt *);

static struct ast_expr *
hack_mem_from_neteffect(struct ast_stmt *);

static struct object *
hack_base_object_from_mem(struct ast_expr *, struct state *);

static struct result
stmt_absexec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_iter_exec(struct ast_stmt *stmt, struct state *state)
{
	/* TODO: check internal consistency of iteration */

	struct ast_stmt *neteffect = iter_neteffect(stmt);
	if (!neteffect) {
		return NULL;
	}

	struct result res = stmt_absexec(neteffect, state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}

	ast_stmt_destroy(neteffect);

	return NULL;
}

static struct ast_expr *
hack_mem_from_neteffect(struct ast_stmt *stmt)
{
	struct ast_stmt *body = ast_stmt_iter_body(stmt);
	assert(ast_stmt_kind(body) == STMT_COMPOUND);
	struct ast_block *block = ast_stmt_as_block(body);
	assert(ast_block_ndecls(block) == 0 && ast_block_nstmts(block) == 1);
	return ast_stmt_as_expr(ast_block_stmts(block)[0]);
}

static struct object *
hack_base_object_from_mem(struct ast_expr *expr, struct state *state)
{
	/* we're currently discarding analysis of `offset` and relying on the
	 * bounds (lower, upper beneath) alone */
	struct ast_expr *acc = ast_expr_memory_root(expr); /* `*(arr+offset)` */
	assert(ast_expr_unary_op(acc) == UNARY_OP_DEREFERENCE);
	struct ast_expr *inner = ast_expr_unary_operand(acc); /* `arr+offset` */
	struct ast_expr *i = ast_expr_identifier_create(dynamic_str("i"));
	assert(ast_expr_equal(ast_expr_binary_e2(inner), i)); 
	ast_expr_destroy(i);
	struct object *obj = lvalue_object(
		expr_lvalue(ast_expr_binary_e1(inner), state)
	);
	assert(obj);
	return obj;
}

/* iter_neteffect */

static struct ast_expr *
stmt_itereval(struct ast_stmt *stmt, struct ast_stmt *iter, struct state *state);

static struct ast_stmt *
iter_neteffect(struct ast_stmt *iter)
{
	struct ast_block *abs = ast_stmt_iter_abstract(iter);
	assert(abs);

	int nstmts = ast_block_nstmts(abs);
	if (!nstmts) {
		return NULL;
	}

	assert(ast_block_ndecls(abs) == 0 && nstmts == 1);

	return ast_stmt_create_iter(
		NULL,
		ast_stmt_copy(ast_stmt_iter_init(iter)),
		ast_stmt_copy(ast_stmt_iter_cond(iter)),
		ast_expr_copy(ast_stmt_iter_iter(iter)),
		ast_block_create(NULL, 0, NULL, 0),
		ast_stmt_create_compound(
			NULL, ast_block_copy(ast_stmt_iter_abstract(iter))
		)
	);
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
stmt_iter_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
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
	case STMT_ITERATION:
		return stmt_iter_itereval(stmt, iter, state);
	default:
		assert(false);
	}
}

static struct ast_expr *
stmt_expr_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_expr *expr = ast_stmt_as_expr(stmt);
	assert(ast_expr_kind(expr) == EXPR_MEMORY || ast_expr_memory_isundefined(expr));
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

	struct object *obj = lvalue_object(expr_lvalue(access, state));
	assert(obj);

	struct ast_expr *lw = ast_stmt_iter_lower_bound(iter),
			*up = ast_stmt_iter_upper_bound(iter);

	bool deallocands = state_range_aredeallocands(state, obj, lw, up);

	object_destroy(obj);

	return deallocands;
}

static struct ast_expr *
hack_access_from_assertion(struct ast_expr *expr)
{
	struct ast_expr *assertand = ast_expr_assertion_assertand(expr);
	assert(ast_expr_kind(assertand) == EXPR_UNARY
			&& ast_expr_unary_op(assertand) == UNARY_OP_DEREFERENCE);
	return assertand;
}


/* stmt_iter_itereval */

static struct ast_expr *
stmt_iter_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state)
{
	assert(false);
}

static struct error *
stmt_jump_exec(struct ast_stmt *stmt, struct state *state)
{
	struct result res = expr_eval(ast_stmt_jump_rv(stmt), state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}
	if (result_hasvalue(res)) {
		struct object *obj = state_getresult(state); 
		assert(obj);
		object_assign(obj, value_copy(result_as_value(res)));
		result_destroy(res);
		state_undeclarevars(state);
	}
	return NULL;
}

/* abstract_audit */

struct ast_block *
hack_flatten_abstract_for_iter_verification(struct ast_function *f,
		struct state* state);

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state,
		struct externals *ext)
{
	struct error *err = NULL;

	if (!state_hasgarbage(actual_state)) {
		return error_create("garbage on heap");
	}
	/*printf("actual: %s\n", state_str(actual_state));*/

	struct state *alleged_state = state_create(ext, ast_function_type(f));
	if ((err = parameterise_state(alleged_state, f))) {
		return err;
	}

	struct ast_block *abs = hack_flatten_abstract_for_iter_verification(
		f, actual_state
	);

	/* mutates alleged_state */
	struct result res = function_absexec(abs, alleged_state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}

	/*printf("actual: %s\n", state_str(actual_state));*/
	/*printf("alleged: %s\n", state_str(alleged_state));*/

	ast_block_destroy(abs);

	bool equiv = state_equal(actual_state, alleged_state);

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
	object_destroy(
		hack_object_from_assertion(
			ast_stmt_sel_cond(stmt), state
		)
	);
	return ast_block_copy(ast_stmt_as_block(ast_stmt_sel_body(stmt)));
}

/* function_absexec */

static struct result
function_absexec(struct ast_block *abs, struct state *state)
{
	int nstmts = ast_block_nstmts(abs);
	struct ast_stmt **stmt = ast_block_stmts(abs);
	for (int i = 0; i < nstmts; i++) {
		struct result res = stmt_absexec(stmt[i], state);
		if (result_iserror(res)) {
			return res;
		}
		result_destroy(res);
	}
	/* wrap result and return */ 
	struct object *obj = state_getresult(state);
	assert(obj);
	return result_value_create(object_as_value(obj));
}

/* stmt_absexec */

static struct result
expr_absexec(struct ast_expr *expr, struct state *state);

static struct result
sel_absexec(struct ast_stmt *stmt, struct state *state);

static struct result
iter_absexec(struct ast_stmt *stmt, struct state *state);

static struct result
comp_absexec(struct ast_stmt *stmt, struct state *state);

static struct result
stmt_absexec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_LABELLED:
		/* ignore labelled statements for now */
		return result_value_create(NULL);
	case STMT_EXPR:
		return expr_absexec(ast_stmt_as_expr(stmt), state);
	case STMT_SELECTION:
		return sel_absexec(stmt, state);
	case STMT_ITERATION:
		return iter_absexec(stmt, state);
	case STMT_COMPOUND:
		return comp_absexec(stmt, state);
	default:
		assert(false);
	}
}

/* expr_absexec */

static struct result
mem_absexec(struct ast_expr *mem, struct state *state);

static struct result
assign_absexec(struct ast_expr *expr, struct state *state);

static struct result
expr_absexec(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_IDENTIFIER:
		assert(ast_expr_memory_isundefined(expr));
		return result_error_create(error_create("undefined"));
	case EXPR_MEMORY:
		return mem_absexec(expr, state);
	case EXPR_ASSIGNMENT:
		return assign_absexec(expr, state);
	default:
		assert(false);
	}
}

static struct result
mem_process(struct ast_expr *red, struct state *state);

static struct result
mem_absexec(struct ast_expr *mem, struct state *state)
{
	struct result res = mem_process(mem, state);
	if (result_iserror(res)) {
		return res;
	}
	if (result_hasvalue(res)) {
		struct object *obj = lvalue_object(
			expr_lvalue(ast_expr_memory_root(mem), state)
		);
		assert(obj);
		object_assign(obj, value_copy(result_as_value(res)));
	}
	return res;
}

/* operates at location level. It either creates an object on the heap and returns
 * a location or gets the location pointed to by an lvalue and attempts to free
 * possibly returning an error
 * */
static struct result
mem_process(struct ast_expr *mem, struct state *state)
{
	if (ast_expr_memory_isundefined(mem)) {
		return result_error_create(error_create("undefined behaviour"));
	}

	struct ast_expr *root = ast_expr_memory_root(mem);

	if (ast_expr_memory_isalloc(mem)) {
		/* assert(strcmp(ast_expr_as_identifier(id), KEYWORD_RESULT) == 0); */

		/* TODO: size needs to be passed in here when added to .alloc */
		return result_value_create(state_alloc(state)); /* XXX */
	}

	assert(ast_expr_memory_isunalloc(mem));

	/* root is pointing at the heap location we want to free, so we want its
	 * value rather than location */
	struct result res = expr_eval(root, state);
	if (result_iserror(res)) {
		return res;
	}
	struct value *val = result_as_value(res);
	assert(val);
	struct error *err = state_dealloc(state, val);
	if (err) {
		fprintf(stderr, "cannot free: %s\n", err->msg);
		assert(false);
	}
	value_destroy(val);
	return result_value_create(NULL);
}

static struct result
assign_absexec(struct ast_expr *expr, struct state *state)
{
	return expr_assign_eval(expr, state);
}

static struct result
sel_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *cond = ast_stmt_sel_cond(stmt);
	if (!expr_decide(cond, state)) {
		struct ast_stmt *nest = ast_stmt_sel_nest(stmt);
		if (nest) {
			return stmt_absexec(nest, state);
		} 
		return result_value_create(NULL);
	}
	return stmt_absexec(ast_stmt_sel_body(stmt), state);
}

static struct result
iter_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct error *err;

	struct ast_expr *mem = hack_mem_from_neteffect(stmt);

	struct object *obj = hack_base_object_from_mem(mem, state);

	struct ast_expr *lw = ast_stmt_iter_lower_bound(stmt),
			*up = ast_stmt_iter_upper_bound(stmt);

	struct result result_lw = expr_eval(lw, state),
	       result_up = expr_eval(up, state);

	if (result_iserror(result_lw)) {
		return result_lw;
	}
	if (result_iserror(result_up)) {
		return result_up;
	}

	struct ast_expr *res_lw = value_to_expr(result_as_value(result_lw)),
			*res_up = value_to_expr(result_as_value(result_up));

	result_destroy(result_up);
	result_destroy(result_lw);

	if (ast_expr_memory_isalloc(mem)) {
		err = state_range_alloc(state, obj, res_lw, res_up);
	} else {
		err = state_range_dealloc(state, obj, res_lw, res_up);
	}
	
	ast_expr_destroy(res_up);
	ast_expr_destroy(res_lw);

	if (err) {
		return result_error_create(err);
	}

	return result_value_create(NULL);
}

static struct result
comp_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct ast_block *b = ast_stmt_as_block(stmt);
	struct ast_stmt **stmts = ast_block_stmts(b);
	for (int i = 0; i < ast_block_nstmts(b); i++) {
		struct result res = stmt_absexec(stmts[i], state);
		if (result_iserror(res)) {
			return res;
		}
	}
	return result_value_create(NULL);
}
