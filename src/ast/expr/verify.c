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
	struct lvalue *root = ast_expr_lvalue(ast_expr_member_root(expr), state);
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

static struct result *
expr_unary_eval(struct ast_expr *expr, struct state *state)
{
	assert(ast_expr_unary_op(expr) == UNARY_OP_DEREFERENCE);

	struct ast_expr *inner = ast_expr_unary_operand(expr); /* arr+offset */

	struct result *res = ast_expr_eval(ast_expr_binary_e1(inner), state);
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

static struct result *
expr_structmember_eval(struct ast_expr *expr, struct state *s)
{
	struct result *res = ast_expr_eval(ast_expr_member_root(expr), s);
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

struct result_arr {
	int n;
	struct result **res;
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
result_arr_append(struct result_arr *arr, struct result *res)
{
	arr->res = realloc(arr->res, sizeof(struct result *) * ++arr->n);
	arr->res[arr->n-1] = res;
}

static struct result_arr *
prepare_arguments(int nargs, struct ast_expr **arg, int nparams,
		struct ast_variable **param, struct state *state);

static struct error *
prepare_parameters(int nparams, struct ast_variable **param, 
		struct result_arr *args, struct state *state);

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

	struct error *err = prepare_parameters(nparams, params, args, state);
	if (err) {
		return result_error_create(err);
	}

	struct result *res = ast_function_absexec(f, state);
	if (result_iserror(res)) {
		return res;
	}
	if (result_hasvalue(res)) { /* preserve value through pop */
		res = result_value_create(value_copy(result_as_value(res)));
	} else {
		res = result_value_create(
			state_vconst(state, ret_type, dynamic_str(name), false)
		);
	}

	state_popframe(state);

	result_arr_destroy(args);

	return res;
}

static struct result *
prepare_argument(struct ast_expr *arg, struct ast_variable *param, struct state *);

static struct result_arr *
prepare_arguments(int nargs, struct ast_expr **arg, int nparams,
		struct ast_variable **param, struct state *state)
{
	assert(nargs == nparams);

	struct result_arr *args = result_arr_create();
	for (int i = 0; i < nargs; i++) {
		result_arr_append(
			args, prepare_argument(arg[i], param[i], state)
		);
	}
	return args;
}

static struct result *
prepare_argument(struct ast_expr *arg, struct ast_variable *p, struct state *s)
{
	if (ast_expr_kind(arg) != EXPR_ARBARG) {
		return ast_expr_eval(arg, s);
	}
	return result_value_create(state_vconst(
		s, ast_variable_type(p), dynamic_str(ast_variable_name(p)), true
	));
}

/* prepare_parameters: Allocate arguments in call expression and assign them to
 * their respective parameters. */
static struct error *
prepare_parameters(int nparams, struct ast_variable **param, 
		struct result_arr *args, struct state *state)
{
	assert(nparams == args->n);

	for (int i = 0; i < args->n; i++) {
		state_declare(state, param[i], true);

		struct result *res = args->res[i];
		if (result_iserror(res)) {
			struct strbuilder *b = strbuilder_create();
			strbuilder_printf(
				b, "param `%s': ",
				ast_variable_name(param[i])
			);
			return error_prepend(
				result_as_error(res), strbuilder_build(b)
			);
		}

		if (!result_hasvalue(res)) {
			continue;
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
	if (result_hasvalue(res)) {
		/*printf("state: %s\n", state_str(state));*/
		struct object *obj = lvalue_object(ast_expr_lvalue(lval, state));
		assert(obj);
		object_assign(obj, value_copy(result_as_value(res)));
	}
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
identifier_assume(char *id, bool value, struct state *state);

static struct preresult *
reduce_assume(struct ast_expr *expr, bool value, struct state *state)
{
	switch (expr->kind) {
	case EXPR_IDENTIFIER:
		return identifier_assume(ast_expr_as_identifier(expr), value, state);
	case EXPR_UNARY:
		assert(ast_expr_unary_op(expr) == UNARY_OP_BANG);
		return reduce_assume(ast_expr_unary_operand(expr), !value, state);
	case EXPR_CALL:
		/* irreducible */
		assert(false);
		props_install(
			state_getprops(state),
			ast_expr_copy(expr)
		);
	default:
		assert(false);
	}
}

static struct preresult *
identifier_assume(char *id, bool value, struct state *state)
{
	/* set value of variable corresponding to identifier != 0 */
	struct object *obj = state_getobject(state, id);
	struct ast_expr *sync = value_as_sync(object_as_value(obj));
	struct value *v = state_getvconst(state, ast_expr_as_identifier(sync));
	bool iscontradiction = value_assume(v, value);
	if (iscontradiction) {
		return preresult_contradiction_create();
	}
	return preresult_empty_create();
}
