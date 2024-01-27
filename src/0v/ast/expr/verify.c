#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "expr.h"
#include "ext.h"
#include "intern.h"
#include "math.h"
#include "object.h"
#include "props.h"
#include "state.h"
#include "util.h"
#include "value.h"
#include "type/type.h"

static bool
expr_unary_decide(struct ast_expr *expr, struct state *state);

static bool
expr_isdeallocand_decide(struct ast_expr *expr, struct state *state);

static bool
expr_binary_decide(struct ast_expr *expr, struct state *state);

bool
ast_expr_decide(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_UNARY:
		return expr_unary_decide(expr, state);
	case EXPR_ISDEALLOCAND:
		return expr_isdeallocand_decide(expr, state);
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
		return !ast_expr_decide(operand, state);
	default:
		assert(false);
	}
}

static bool
unary_rangedecide(struct ast_expr *expr, struct ast_expr *lw,
		struct ast_expr *up, struct state *);

static bool
expr_isdeallocand_rangedecide(struct ast_expr *expr, struct ast_expr *lw,
		struct ast_expr *up, struct state *);

bool
ast_expr_rangedecide(struct ast_expr *expr, struct ast_expr *lw,
		struct ast_expr *up, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_UNARY:
		/* recurses for `!@` cases */
		return unary_rangedecide(expr, lw, up, state);
	case EXPR_ISDEALLOCAND:
		return expr_isdeallocand_rangedecide(expr, lw, up, state);
	default:
		assert(false);
	}
}

static bool
unary_rangedecide(struct ast_expr *expr, struct ast_expr *lw,
		struct ast_expr *up, struct state *state)
{
	struct ast_expr *operand = ast_expr_unary_operand(expr);
	switch (ast_expr_unary_op(expr)) {
	case UNARY_OP_BANG:
		return !ast_expr_rangedecide(operand, lw, up, state);
	default:
		assert(false);
	}
}

static bool
expr_isdeallocand_rangedecide(struct ast_expr *expr, struct ast_expr *lw,
		struct ast_expr *up, struct state *state)
{
	struct ast_expr *acc = ast_expr_isdeallocand_assertand(expr); /* `*(arr+offset)` */

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
		ast_expr_lvalue(ast_expr_binary_e1(acc), state)
	);
	assert(obj);

	return state_range_aredeallocands(state, obj, lw, up);
}

struct error *
ast_expr_exec(struct ast_expr *expr, struct state *state)
{
	struct result *res = ast_expr_eval(expr, state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}
	result_destroy(res);
	return NULL;
}

struct lvalue *
expr_identifier_lvalue(struct ast_expr *expr, struct state *state);

struct lvalue *
expr_unary_lvalue(struct ast_expr *expr, struct state *state);

struct lvalue *
expr_structmember_lvalue(struct ast_expr *expr, struct state *state);

struct lvalue *
ast_expr_lvalue(struct ast_expr *expr, struct state *state)
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

	struct lvalue *root = ast_expr_lvalue(ast_expr_binary_e1(inner), state);
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
	struct ast_expr *root = ast_expr_member_root(expr);
	struct lvalue *root_lval = ast_expr_lvalue(root, state);
	struct object *root_obj = lvalue_object(root_lval);
	assert(root_obj);
	char *field = ast_expr_member_field(expr);
	struct object *member = object_getmember(
		root_obj, lvalue_type(root_lval), field, state
	);
	if (!member) {
		/* TODO: lvalue error */
		return lvalue_create(NULL, NULL);
	}
	struct ast_type *t = object_getmembertype(
		root_obj, lvalue_type(root_lval), field, state
	);
	assert(t);
	return lvalue_create(t, member);
}


static struct object *
hack_object_from_assertion(struct ast_expr *expr, struct state *state)
{	
	/* get assertand */
	struct ast_expr *assertand = ast_expr_isdeallocand_assertand(expr);

	/* get `assertand' variable */
	struct object *obj = lvalue_object(ast_expr_lvalue(assertand, state));
	assert(obj);
	return obj;
}

static bool
expr_isdeallocand_decide(struct ast_expr *expr, struct state *state)
{
	struct object *obj = hack_object_from_assertion(expr, state);
	bool isdeallocand = state_addresses_deallocand(state, obj);
	return isdeallocand;
}

struct result *
ast_expr_eval(struct ast_expr *expr, struct state *state);

static bool
value_compare(struct value *, enum ast_binary_operator, struct value *);

static bool
expr_binary_decide(struct ast_expr *expr, struct state *state)
{
	struct result *root = ast_expr_eval(ast_expr_binary_e1(expr), state),
		      *last = ast_expr_eval(ast_expr_binary_e2(expr), state);

	assert(!result_iserror(root) && !result_iserror(last));

	/*printf("state: %s\n", state_str(state));*/
	/*printf("expr: %s\n", ast_expr_str(expr));*/
	return value_compare(
		result_as_value(root),
		ast_expr_binary_op(expr),
		result_as_value(last)
	);
}

static bool
value_compare(struct value *v1, enum ast_binary_operator op, struct value *v2)
{
	switch (op) {
	case BINARY_OP_EQ:
		return value_equal(v1, v2);
	case BINARY_OP_NE:
		return !value_compare(v1, BINARY_OP_EQ, v2);
	default:
		assert(false);
	}
}

/* ast_expr_eval */

static struct result *
expr_constant_eval(struct ast_expr *expr, struct state *state);

static struct result *
expr_literal_eval(struct ast_expr *expr, struct state *state);

static struct result *
expr_identifier_eval(struct ast_expr *expr, struct state *state);

static struct result *
expr_unary_eval(struct ast_expr *expr, struct state *state);

static struct result *
expr_structmember_eval(struct ast_expr *expr, struct state *state);

static struct result *
expr_call_eval(struct ast_expr *expr, struct state *state);

static struct result *
expr_assign_eval(struct ast_expr *expr, struct state *state);

static struct result *
expr_incdec_eval(struct ast_expr *expr, struct state *state);

static struct result *
expr_binary_eval(struct ast_expr *expr, struct state *state);

static struct result *
arbarg_eval(struct ast_expr *expr, struct state *state);

struct result *
ast_expr_eval(struct ast_expr *expr, struct state *state)
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
	case EXPR_ARBARG:
		return arbarg_eval(expr, state);
	default:
		assert(false);
	}
}

static struct result *
expr_literal_eval(struct ast_expr *expr, struct state *state)
{
	return result_value_create(
		value_literal_create(ast_expr_as_literal(expr))
	);
}

static struct result *
expr_constant_eval(struct ast_expr *expr, struct state *state)
{
	return result_value_create(
		value_int_create(ast_expr_as_constant(expr))
	);
}

static struct result *
expr_identifier_eval(struct ast_expr *expr, struct state *state)
{
	if (state_getvconst(state, ast_expr_as_identifier(expr))) {
		return result_value_create(
			value_sync_create(ast_expr_copy(expr))
		);
	}

	char *id = ast_expr_as_identifier(expr);
	struct object *obj = state_getobject(state, id);
	if (!obj) {
		struct strbuilder *b = strbuilder_create();
		strbuilder_printf(b, "unknown idenitfier `%s'", id);
		return result_error_create(error_create(strbuilder_build(b)));
	}
	struct value *val = object_as_value(obj);
	if (!val) {
		printf("state: %s\n", state_str(state));
		struct strbuilder *b = strbuilder_create();
		strbuilder_printf(b, "`%s' has no value", id);
		return result_error_create(error_create(strbuilder_build(b)));
	}
	return result_value_create(value_copy(val));
}

static struct ast_expr *
expr_to_binary(struct ast_expr *expr);

static struct result *
binary_deref_eval(struct ast_expr *expr, struct state *state);

static struct result *
expr_unary_eval(struct ast_expr *expr, struct state *state)
{
	assert(ast_expr_unary_op(expr) == UNARY_OP_DEREFERENCE);

	struct ast_expr *binary = expr_to_binary(ast_expr_unary_operand(expr));
	struct result *res = binary_deref_eval(binary, state);
	ast_expr_destroy(binary);
	return res;
}

static struct ast_expr *
expr_to_binary(struct ast_expr *expr)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_BINARY:
		return ast_expr_copy(expr);
	default:
		return ast_expr_binary_create(
			ast_expr_copy(expr),
			BINARY_OP_ADDITION,
			ast_expr_constant_create(0)
		);
	}
}

static struct result *
binary_deref_eval(struct ast_expr *expr, struct state *state)
{
	struct result *res = ast_expr_eval(ast_expr_binary_e1(expr), state);
	if (result_iserror(res)) {
		return res;
	}
	struct value *arr = result_as_value(res);
	assert(arr);
	struct object *obj = state_deref(state, arr, ast_expr_binary_e2(expr));
	assert(obj);
	result_destroy(res);

	struct value *v = object_as_value(obj);
	assert(v);

	return result_value_create(value_copy(v));
}

static struct result *
expr_structmember_eval(struct ast_expr *expr, struct state *s)
{
	struct ast_expr *root = ast_expr_member_root(expr);
	struct result *res = ast_expr_eval(root, s);
	if (result_iserror(res)) {
		return res;
	}
	char *field = ast_expr_member_field(expr);
	struct object *member = value_struct_member(result_as_value(res), field);
	if (!member) {
		struct strbuilder *b = strbuilder_create();
		char *root_str = ast_expr_str(root);
		strbuilder_printf(
			b, "`%s' has no field `%s'", root_str, field
		);
		free(root_str);
		return result_error_create(error_create(strbuilder_build(b)));
	}
	struct value *obj_value = object_as_value(member);
	/* XXX */
	struct value *v = obj_value ? value_copy(obj_value) : NULL;
	result_destroy(res);
	return result_value_create(v);
}

/* expr_call_eval */

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
result_arr_append(struct result_arr *arr, struct result *res)
{
	arr->res = realloc(arr->res, sizeof(struct result *) * ++arr->n);
	arr->res[arr->n-1] = res;
}

struct result_arr *
prepare_arguments(int nargs, struct ast_expr **arg, int nparams,
		struct ast_variable **param, struct state *state);

struct error *
prepare_parameters(int nparams, struct ast_variable **param, 
		struct result_arr *args, char *fname, struct state *state);

static struct result *
call_absexec(struct ast_expr *call, struct ast_function *, struct state *);

static struct result *
pf_augment(struct value *v, struct ast_expr *root, struct state *);

static struct result *
expr_call_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *root = ast_expr_call_root(expr);
	/* TODO: function-valued-expressions */
	char *name = ast_expr_as_identifier(root);

	struct ast_function *f = externals_getfunc(state_getext(state), name);
	if (!f) {
		struct strbuilder *b = strbuilder_create();
		strbuilder_printf(b, "function `%s' not found", name);
		return result_error_create(error_create(strbuilder_build(b)));
	}
	int nparams = ast_function_nparams(f);
	struct ast_variable **params = ast_function_params(f);

	struct result_arr *args = prepare_arguments(
		ast_expr_call_nargs(expr),
		ast_expr_call_args(expr),
		nparams, params, state
	);

	struct ast_type *ret_type = ast_function_type(f);
	state_pushframe(state, dynamic_str(name), ret_type);

	struct error *err = prepare_parameters(
		nparams, params, args, name, state
	);
	if (err) {
		return result_error_create(err);
	}

	struct result *res = call_absexec(expr, f, state);
	if (result_iserror(res)) {
		return res;
	}
	/* copy to preserve value through popping of frame */
	struct value *v = NULL;
	if (result_hasvalue(res)) {
		v = value_copy(result_as_value(res));
	}

	state_popframe(state);
	result_arr_destroy(args);

	if (v) {
		return pf_augment(v, expr, state);
	}

	return res;
}

static struct result *
call_arbitraryresult(struct ast_expr *call, struct ast_function *, struct state *);

static struct result *
call_absexec(struct ast_expr *expr, struct ast_function *f, struct state *state)
{
	struct result *res = ast_function_absexec(f, state);
	if (result_iserror(res) || result_hasvalue(res)) {
		return res;
	}
	return call_arbitraryresult(expr, f, state);
}

static struct result *
pf_augment(struct value *v, struct ast_expr *call, struct state *state)
{
	if (!value_isstruct(v)) {
		return result_value_create(value_copy(v));
	}
	struct result *res = ast_expr_pf_reduce(call, state);
	if (result_iserror(res)) {
		return res;
	}
	assert(result_hasvalue(res));
	return result_value_create(
		value_pf_augment(v, value_as_sync(result_as_value(res)))
	);
}

static struct result *
call_to_computed_value(struct ast_function *, struct state *s);

static struct result *
call_arbitraryresult(struct ast_expr *expr, struct ast_function *f,
		struct state *state)
{
	struct result *res = call_to_computed_value(f, state);
	if (result_iserror(res)) {
		return res;
	}
	assert(result_hasvalue(res));
	return res;
}

static struct result *
call_to_computed_value(struct ast_function *f, struct state *s)
{
	/* TODO: function-valued root */
	char *root = ast_function_name(f);

	int nparams = ast_function_nparams(f);
	struct ast_variable **uncomputed_param = ast_function_params(f);
	struct ast_expr **computed_param = malloc(
		sizeof(struct ast_expr *) * nparams
	);
	for (int i = 0; i < nparams; i++) {
		struct ast_expr *param = ast_expr_identifier_create(
			dynamic_str(ast_variable_name(uncomputed_param[i]))
		);
		struct result *res = ast_expr_eval(param, s);
		ast_expr_destroy(param);
		if (result_iserror(res)) {
			return res;
		}
		assert(result_hasvalue(res));
		struct value *v = result_as_value(res);
		if (value_issync(v)) {
			computed_param[i] = value_as_sync(result_as_value(res));
		} else {
			computed_param[i] = ast_expr_identifier_create(
				dynamic_str(ast_variable_name(uncomputed_param[i]))
			);
		}
	}

	return result_value_create(
		value_sync_create(
			ast_expr_call_create(
				ast_expr_identifier_create(dynamic_str(root)),
				nparams, computed_param
			)
		)
	);
}

struct result_arr *
prepare_arguments(int nargs, struct ast_expr **arg, int nparams,
		struct ast_variable **param, struct state *state)
{
	assert(nargs == nparams);

	struct result_arr *args = result_arr_create();
	for (int i = 0; i < nargs; i++) {
		result_arr_append(args, ast_expr_eval(arg[i], state));
	}
	return args;
}

/* prepare_parameters: Allocate arguments in call expression and assign them to
 * their respective parameters. */
struct error *
prepare_parameters(int nparams, struct ast_variable **param, 
		struct result_arr *args, char *fname, struct state *state)
{
	assert(nparams == args->n);

	for (int i = 0; i < args->n; i++) {
		state_declare(state, param[i], true);

		struct result *res = args->res[i];
		if (result_iserror(res)) {
			return result_as_error(res);
		}

		if (!result_hasvalue(res)) {
			struct strbuilder *b = strbuilder_create();
			strbuilder_printf(
				b, "parameter `%s' of function `%s' has no value",
				ast_variable_name(param[i]), fname
			);
			return error_create(strbuilder_build(b));
		}

		struct ast_expr *name = ast_expr_identifier_create(
			dynamic_str(ast_variable_name(param[i]))
		);
		struct object *obj = lvalue_object(ast_expr_lvalue(name, state));
		ast_expr_destroy(name);

		object_assign(obj, value_copy(result_as_value(res)));
	}
	return NULL;
}

static struct result *
expr_assign_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *lval = ast_expr_assignment_lval(expr),
			*rval = ast_expr_assignment_rval(expr);

	struct result *res = ast_expr_eval(rval, state);
	if (result_iserror(res)) {
		return res;
	}
	if (!result_hasvalue(res)) {
		struct strbuilder *b = strbuilder_create();
		char *s = ast_expr_str(rval);
		strbuilder_printf(b, "`%s' has no value", s);
		free(s);
		return result_error_create(error_create(strbuilder_build(b)));
	}
	struct object *obj = lvalue_object(ast_expr_lvalue(lval, state));
	if (!obj) {
		struct strbuilder *b = strbuilder_create();
		char *s = ast_expr_str(lval);
		strbuilder_printf(b, "`%s' is not a valid object", s);
		free(s);
		return result_error_create(error_create(strbuilder_build(b)));
	}
	object_assign(obj, value_copy(result_as_value(res)));
	return res;
}

static struct result *
expr_incdec_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *assign = ast_expr_incdec_to_assignment(expr);

	struct result *res;

	if (ast_expr_incdec_pre(expr)) { /* ++i */
		res = expr_assign_eval(assign, state);
	} else { /* i++ */
		res = ast_expr_eval(ast_expr_incdec_root(expr), state);
		/* assign and ignore result */ 
		result_destroy(expr_assign_eval(assign, state));
	}

	ast_expr_destroy(assign);

	return res;
}

static struct result *
expr_binary_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *e1 = ast_expr_binary_e1(expr),
			*e2 = ast_expr_binary_e2(expr);
	struct result *res1 = ast_expr_eval(e1, state),
		      *res2 = ast_expr_eval(e2, state);
	if (result_iserror(res1)) {
		return res1;
	}
	if (result_iserror(res2)) {
		return res2;
	}
	return result_value_create(
		value_sync_create(
			ast_expr_binary_create(
				value_to_expr(result_as_value(res1)),
				ast_expr_binary_op(expr),
				value_to_expr(result_as_value(res2))
			)
		)
	);
}

static struct result *
arbarg_eval(struct ast_expr *expr, struct state *state)
{
	return result_value_create(state_vconst(
		state,
		/* XXX: we will investigate type conversions later */
		ast_type_create_ptr(ast_type_create(TYPE_VOID, 0)),
		NULL,
		false
	));
}

static struct result *
assign_absexec(struct ast_expr *expr, struct state *state);

struct result *
ast_expr_absexec(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_ASSIGNMENT:
		return assign_absexec(expr, state);
	default:
		assert(false);
	}
}

static struct result *
assign_absexec(struct ast_expr *expr, struct state *state)
{
	return expr_assign_eval(expr, state);
}

static struct preresult *
reduce_assume(struct ast_expr *, bool value, struct state *);

struct preresult *
ast_expr_assume(struct ast_expr *expr, struct state *state)
{
	return reduce_assume(expr, true, state);
}

static struct preresult *
identifier_assume(struct ast_expr *expr, bool value, struct state *state);

static struct preresult *
ast_expr_pf_reduce_assume(struct ast_expr *, bool value, struct state *);

static struct preresult *
irreducible_assume(struct ast_expr *, bool value, struct state *);

static struct preresult *
binary_assume(struct ast_expr *expr, bool value, struct state *);

static struct preresult *
reduce_assume(struct ast_expr *expr, bool value, struct state *s)
{
	switch (expr->kind) {
	case EXPR_IDENTIFIER:
		return identifier_assume(expr, value, s);
	case EXPR_UNARY:
		assert(ast_expr_unary_op(expr) == UNARY_OP_BANG);
		return reduce_assume(ast_expr_unary_operand(expr), !value, s);
	case EXPR_BRACKETED:
		return reduce_assume(expr->root, value, s);
	case EXPR_CALL:
	case EXPR_STRUCTMEMBER:
		return ast_expr_pf_reduce_assume(expr, value, s);
	case EXPR_BINARY:
		return binary_assume(expr, value, s);
	default:
		assert(false);
	}
}

static struct preresult *
identifier_assume(struct ast_expr *expr, bool value, struct state *s)
{
	struct state *s_copy = state_copy(s);
	struct result *res = ast_expr_eval(expr, s_copy);

	/* TODO: user errors */
	assert(!result_iserror(res) && result_hasvalue(res));

	state_destroy(s_copy);

	return irreducible_assume(value_as_sync(result_as_value(res)), value, s);
}

static struct preresult *
ast_expr_pf_reduce_assume(struct ast_expr *expr, bool value, struct state *s)
{
	struct result *res = ast_expr_pf_reduce(expr, s);
	/* TODO: user errors */
	assert(!result_iserror(res) && result_hasvalue(res));

	return irreducible_assume(value_as_sync(result_as_value(res)), value, s);
}

static struct result *
binary_pf_reduce(struct ast_expr *e1, enum ast_binary_operator,
		struct ast_expr *e2, struct state *);

static struct result *
unary_pf_reduce(struct ast_expr *, struct state *);

static struct result *
call_pf_reduce(struct ast_expr *, struct state *);

static struct result *
structmember_pf_reduce(struct ast_expr *, struct state *);

struct result *
ast_expr_pf_reduce(struct ast_expr *e, struct state *s)
{
	switch (ast_expr_kind(e)) {
	case EXPR_CONSTANT:
	case EXPR_STRING_LITERAL:
	case EXPR_IDENTIFIER:
		return ast_expr_eval(e, s);
	case EXPR_UNARY:
		return unary_pf_reduce(e, s);
	case EXPR_BINARY:
		return binary_pf_reduce(
			ast_expr_binary_e1(e),
			ast_expr_binary_op(e),
			ast_expr_binary_e2(e),
			s
		);
	case EXPR_CALL:
		return call_pf_reduce(e, s);
	case EXPR_STRUCTMEMBER:
		return structmember_pf_reduce(e, s);
	default:
		assert(false);
	}
}

static struct result *
unary_pf_reduce(struct ast_expr *e, struct state *s)
{
	/* TODO: reduce by actually dereferencing if expr is a deref and this is
	 * possible in the current state */
	struct result *res = ast_expr_pf_reduce(ast_expr_unary_operand(e), s);
	if (result_iserror(res)) {
		return res;
	}
	assert(result_hasvalue(res));
	return result_value_create(
		value_sync_create(
			ast_expr_unary_create(
				value_as_sync(result_as_value(res)),
				ast_expr_unary_op(e)
			)
		)
	);
}

static struct result *
binary_pf_reduce(struct ast_expr *e1, enum ast_binary_operator op,
		struct ast_expr *e2, struct state *s)
{
	struct result *res1 = ast_expr_pf_reduce(e1, s);
	if (result_iserror(res1)) {
		return res1;
	}
	assert(result_hasvalue(res1));
	struct result *res2 = ast_expr_pf_reduce(e2, s);
	if (result_iserror(res2)) {
		return res2;
	}
	assert(result_hasvalue(res2));
	return result_value_create(
		value_sync_create(
			ast_expr_binary_create(
				value_to_expr(result_as_value(res1)),
				op,
				value_to_expr(result_as_value(res2))
			)
		)
	);
}

static struct result *
call_pf_reduce(struct ast_expr *e, struct state *s)
{
	/* TODO: allow for exprs as root */
	char *root = ast_expr_as_identifier(ast_expr_call_root(e));

	int nargs = ast_expr_call_nargs(e);
	struct ast_expr **unreduced_arg = ast_expr_call_args(e);
	struct ast_expr **reduced_arg = malloc(sizeof(struct ast_expr *) *nargs);
	for (int i = 0; i < nargs; i++) {
		struct result *res = ast_expr_pf_reduce(unreduced_arg[i], s);
		if (result_iserror(res)) {
			return res;
		}
		assert(result_hasvalue(res));
		reduced_arg[i] = ast_expr_copy(value_to_expr(result_as_value(res)));
	}
	return result_value_create(
		value_sync_create(
			ast_expr_call_create(
				ast_expr_identifier_create(dynamic_str(root)),
				nargs, reduced_arg
			)
		)
	);
}

static struct result *
structmember_pf_reduce(struct ast_expr *expr, struct state *s)
{
	struct result *res = ast_expr_pf_reduce(ast_expr_member_root(expr), s);
	if (result_iserror(res)) {
		return res;
	}
	assert(result_hasvalue(res));
	char *field = ast_expr_member_field(expr);
	struct value *v = result_as_value(res);
	if (value_isstruct(v)) {
		struct object *obj = value_struct_member(v, field);
		struct value *obj_value = object_as_value(obj);
		assert(obj_value);
		return result_value_create(value_copy(obj_value));
	}
	assert(value_issync(v));
	return result_value_create(
		value_sync_create(
			ast_expr_member_create(
				value_as_sync(v),
				dynamic_str(field)
			)
		)
	);
}


static struct preresult *
irreducible_assume_actual(struct ast_expr *e, struct state *s);

static struct preresult *
irreducible_assume(struct ast_expr *e, bool value, struct state *s)
{
	struct ast_expr *prop = ast_expr_inverted_copy(e, !value);
	struct preresult *r = irreducible_assume_actual(prop, s);
	ast_expr_destroy(prop);
	return r;
}

static struct preresult *
irreducible_assume_actual(struct ast_expr *e, struct state *s)
{
	struct props *p = state_getprops(s);
	if (props_contradicts(p, e)) {
		return preresult_contradiction_create();
	}
	props_install(state_getprops(s), ast_expr_copy(e));
	return preresult_empty_create();
}

static struct preresult *
binary_assume(struct ast_expr *expr, bool value, struct state *s)
{
	struct result *r1 = ast_expr_pf_reduce(expr->u.binary.e1, s),
		      *r2 = ast_expr_pf_reduce(expr->u.binary.e2, s);

	/* TODO: user errors */
	struct value *v1 = result_as_value(r1),
		     *v2 = result_as_value(r2);

	return irreducible_assume(
		ast_expr_binary_create(
			value_to_expr(v1),
			expr->u.binary.op,
			value_to_expr(v2)
		),
		value,
		s
	);
}
