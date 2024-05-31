#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "breakpoint.h"
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
expr_identifier_decide(struct ast_expr *expr, struct state *state);

static bool
expr_isdeallocand_decide(struct ast_expr *expr, struct state *state);

static bool
expr_binary_decide(struct ast_expr *expr, struct state *state);

bool
ast_expr_decide(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_CONSTANT:
		return (bool) ast_expr_as_constant(expr);
	case EXPR_IDENTIFIER:
		return expr_identifier_decide(expr, state);
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
expr_identifier_decide(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *prop = ast_expr_binary_create(
		ast_expr_copy(expr), BINARY_OP_NE, ast_expr_constant_create(0)
	);
	bool res = ast_expr_decide(prop, state);
	ast_expr_destroy(prop);
	return res;
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

	struct l_res *res = ast_expr_lvalue(ast_expr_binary_e1(acc), state);
	if (l_res_iserror(res)) {
		assert(false);
	}	
	struct object *obj = lvalue_object(l_res_as_lvalue(res));
	assert(obj);

	return state_range_aredeallocands(state, obj, lw, up);
}

static struct error *
rangeprocess_alloc(struct ast_expr *, struct ast_expr *lw, struct ast_expr *up, struct state *);

static struct error *
rangeprocess_dealloc(struct ast_expr *, struct ast_expr *lw, struct ast_expr *up, struct state *);

static struct object *
hack_base_object_from_alloc(struct ast_expr *, struct state *);

struct error *
ast_expr_alloc_rangeprocess(struct ast_expr *alloc, struct ast_expr *lw, struct ast_expr *up,
		struct state *state)
{
	struct error *err;
	
	struct r_res *r_res_lw = ast_expr_eval(lw, state),
		      *r_res_up = ast_expr_eval(up, state);

	if (r_res_iserror(r_res_lw)) {
		return r_res_as_error(r_res_lw);
	}
	if (r_res_iserror(r_res_up)) {
		return r_res_as_error(r_res_up);
	}

	/*printf("stmt: %s\n", ast_stmt_str(stmt));*/
	/*printf("state: %s\n", state_str(state));*/

	struct ast_expr *res_lw = value_to_expr(rvalue_value(r_res_as_rvalue(r_res_lw))),
			*res_up = value_to_expr(rvalue_value(r_res_as_rvalue(r_res_up)));

	r_res_destroy(r_res_up);
	r_res_destroy(r_res_lw);

	switch (alloc->kind) {
	case EXPR_ASSIGNMENT:
		err = rangeprocess_alloc(alloc, res_lw, res_up, state);
		break;
	case EXPR_ALLOCATION:
		err = rangeprocess_dealloc(alloc, res_lw, res_up, state);
		break;
	default:
		assert(false);
	}

	ast_expr_destroy(res_up);
	ast_expr_destroy(res_lw);

	if (err) {
		return err;
	}
	return NULL;
}

static struct error *
rangeprocess_alloc(struct ast_expr *expr, struct ast_expr *lw, struct ast_expr *up,
		struct state *state)
{
	struct ast_expr *lval = ast_expr_assignment_lval(expr),
			*rval = ast_expr_assignment_rval(expr);
	assert(ast_expr_kind(rval) == EXPR_ALLOCATION);
	assert(ast_expr_alloc_kind(rval) != DEALLOC);
	struct object *obj = hack_base_object_from_alloc(lval, state);
	return state_range_alloc(state, obj, lw, up);
}

static struct error *
rangeprocess_dealloc(struct ast_expr *dealloc, struct ast_expr *lw, struct ast_expr *up,
		struct state *state)
{
	struct object *obj = hack_base_object_from_alloc(ast_expr_alloc_arg(dealloc), state);
	return state_range_dealloc(state, obj, lw, up);
}

static struct object *
hack_base_object_from_alloc(struct ast_expr *expr, struct state *state)
{
	/* we're currently discarding analysis of `offset` and relying on the
	 * bounds (lower, upper beneath) alone. We passed in `*(arr+offset)` */
	struct ast_expr *inner = ast_expr_unary_operand(expr); /* `arr+offset` */
	struct ast_expr *i = ast_expr_identifier_create(dynamic_str("i"));
	assert(ast_expr_equal(ast_expr_binary_e2(inner), i)); 
	ast_expr_destroy(i);
	struct l_res *res = ast_expr_lvalue(ast_expr_binary_e1(inner), state);
	if (l_res_iserror(res)) {
		assert(false);
	}
	struct object *obj = lvalue_object(l_res_as_lvalue(res));
	assert(obj);
	return obj;
}

struct error *
ast_expr_exec(struct ast_expr *expr, struct state *state)
{
	struct r_res *res = ast_expr_eval(expr, state);
	if (r_res_iserror(res)) {
		return r_res_as_error(res);
	}
	/* r_res_destroy(res); */
	return NULL;
}

static struct l_res *
expr_identifier_lvalue(struct ast_expr *expr, struct state *state);

static struct l_res *
expr_unary_lvalue(struct ast_expr *expr, struct state *state);

static struct l_res *
expr_structmember_lvalue(struct ast_expr *expr, struct state *state);

struct l_res *
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

static struct l_res *
expr_identifier_lvalue(struct ast_expr *expr, struct state *state)
{
	char *id = ast_expr_as_identifier(expr);

	struct object_res *res = state_getobject(state, id);
	if (object_res_iserror(res)) {
		return l_res_error_create(object_res_as_error(res));
	}

	return l_res_lvalue_create(
		lvalue_create(
			state_getvariabletype(state, id),
			object_res_as_object(res)
		)
	);
}

static struct l_res *
expr_unary_lvalue(struct ast_expr *expr, struct state *state)
{
	assert(ast_expr_unary_op(expr) == UNARY_OP_DEREFERENCE);

	struct r_res *res = ast_expr_eval(
		ast_expr_unary_operand(expr), state
	);
	if (r_res_iserror(res)) {
		return l_res_error_create(r_res_as_error(res));
	}
	struct rvalue *rval = r_res_as_rvalue(res);
	struct value *v = rvalue_value(rval);

	struct object_res *deref_res = state_deref(state, v);
	if (object_res_iserror(deref_res)) {
		struct error *err = object_res_as_error(deref_res);
		if (error_to_state_deref_rconst(err)) {
			char *s = ast_expr_str(expr);
			struct error *e = error_printf(
				"undefined indirection: %s is rconst not lvalue", s
			);
			free(s);
			return l_res_error_create(e);
		}
		return l_res_error_create(err);
	}
	struct object *obj = object_res_as_object(deref_res);
	if (!obj) {
		char *s = ast_expr_str(expr);
		struct error *e = error_printf(
			"undefined indirection: %s is not an lvalue", s
		);
		free(s);
		return l_res_error_create(e);
	}

	/* dereference the type */
	struct ast_type *t = ast_type_ptr_type(rvalue_type(rval));

	return l_res_lvalue_create(lvalue_create(ast_type_copy(t), obj));
}

static struct l_res *
expr_structmember_lvalue(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *root = ast_expr_member_root(expr);
	struct l_res *root_res = ast_expr_lvalue(root, state);
	if (l_res_iserror(root_res)) {
		return root_res;
	}
	struct lvalue *root_lval = l_res_as_lvalue(root_res);
	struct object *root_obj = lvalue_object(root_lval);
	assert(root_obj);
	char *field = ast_expr_member_field(expr);
	struct object *member = object_getmember(
		root_obj, lvalue_type(root_lval), field, state
	);
	if (!member) {
		char *root_str = ast_expr_str(root);
		struct error *e = error_printf(
			"`%s' has no field `%s'", root_str, field
		);
		free(root_str);
		return l_res_error_create(e);
	}
	struct ast_type *t = object_getmembertype(
		root_obj, lvalue_type(root_lval), field, state
	);
	assert(t);
	return l_res_lvalue_create(lvalue_create(t, member));
}

static struct object *
hack_object_from_assertion(struct ast_expr *expr, struct state *state)
{	
	/* get assertand */
	struct ast_expr *assertand = ast_expr_isdeallocand_assertand(expr);

	/* get `assertand' variable */
	struct l_res *res = ast_expr_lvalue(assertand, state);
	if (l_res_iserror(res)) {
		assert(false);
	}
	struct object *obj = lvalue_object(l_res_as_lvalue(res));
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

static bool
value_compare(struct value *, enum ast_binary_operator, struct value *);

static bool
expr_binary_decide(struct ast_expr *expr, struct state *state)
{
	struct r_res *root = ast_expr_eval(ast_expr_binary_e1(expr), state),
		      *last = ast_expr_eval(ast_expr_binary_e2(expr), state);

	assert(!r_res_iserror(root) && !r_res_iserror(last));

	return value_compare(
		rvalue_value(r_res_as_rvalue(root)),
		ast_expr_binary_op(expr),
		rvalue_value(r_res_as_rvalue(last))
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

static struct r_res *
expr_constant_eval(struct ast_expr *expr, struct state *state);

static struct r_res *
expr_literal_eval(struct ast_expr *expr, struct state *state);

static struct r_res *
expr_identifier_eval(struct ast_expr *expr, struct state *state);

static struct r_res *
expr_unary_eval(struct ast_expr *expr, struct state *state);

static struct r_res *
expr_structmember_eval(struct ast_expr *expr, struct state *state);

static struct r_res *
expr_call_eval(struct ast_expr *expr, struct state *state);

static struct r_res *
expr_assign_eval(struct ast_expr *expr, struct state *state);

static struct r_res *
expr_incdec_eval(struct ast_expr *expr, struct state *state);

static struct r_res *
expr_binary_eval(struct ast_expr *expr, struct state *state);

static struct r_res *
arbarg_eval(struct ast_expr *expr, struct state *state);

struct r_res *
ast_expr_eval(struct ast_expr *expr, struct state *state)
{
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
	case EXPR_BRACKETED:
		return ast_expr_eval(ast_expr_bracketed_root(expr), state);
	default:
		assert(false);
	}
}

static struct r_res *
expr_literal_eval(struct ast_expr *expr, struct state *state)
{
	return r_res_rvalue_create(
		rvalue_create(
			ast_type_create_char(),
			state_static_init(state, expr)
		)
	);
}

static struct r_res *
expr_constant_eval(struct ast_expr *expr, struct state *state)
{
	return r_res_rvalue_create(
		rvalue_create(
			ast_type_create_int(),
			value_int_create(ast_expr_as_constant(expr))
		)
	);
}

static struct r_res *
hack_identifier_builtin_eval(char *id, struct state *state);

static struct r_res *
arr_identifier_eval(char *, struct state *);

static struct r_res *
nonarr_identifier_eval(char *, struct state *);

static struct r_res *
expr_identifier_eval(struct ast_expr *expr, struct state *state)
{
	struct r_res *res = hack_identifier_builtin_eval(
		ast_expr_as_identifier(expr), state
	);
	if (!r_res_iserror(res) && r_res_hasrvalue(res)) {
		return res;
	}

	char *id = ast_expr_as_identifier(expr);

	/* XXX */
	if (id[0] == '#') {
		assert(false);
		return r_res_rvalue_create(
			rvalue_create(
				ast_type_create_char(),
				value_literal_create(id)
			)
		);
	}

	/* TODO: check that id exists in state */

	return ast_type_isarr(state_getvariabletype(state, id))
		? arr_identifier_eval(id, state)
		: nonarr_identifier_eval(id, state);
}

static struct r_res *
hack_identifier_builtin_eval(char *id, struct state *state)
{
	if (state_getvconst(state, id) || strncmp(id, "ptr:", 4) == 0) {
		/* TODO set type from vconsts */
		return r_res_rvalue_create(
			rvalue_create(
				ast_type_create_voidptr(),
				value_sync_create(ast_expr_identifier_create(dynamic_str(id)))
			)
		);
	}
	return r_res_error_create(error_printf("not built-in"));
}

static struct r_res *
arr_identifier_eval(char *id, struct state *state)
{
	struct value *v = state_getloc(state, id);
	assert(v);
	/* array types are converted to pointer to first elem */ 
	struct ast_type *t = ast_type_create_ptr(
		ast_type_arr_type(state_getvariabletype(state, id))
	);
	return r_res_rvalue_create(
		rvalue_create(ast_type_copy(t), value_copy(v))
	);
}

static struct r_res *
nonarr_identifier_eval(char *id, struct state *state)
{
	struct object_res *obj_res = state_getobject(state, id);
	if (object_res_iserror(obj_res)) {
		return r_res_error_create(object_res_as_error(obj_res));
	}
	struct object *obj = object_res_as_object(obj_res);
	struct value *val = object_as_value(obj);
	if (!val) {
		return r_res_error_create(
			error_printf("undefined memory access: `%s' has no value", id)
		);
	}
	return r_res_rvalue_create(
		rvalue_create(
			ast_type_copy(state_getvariabletype(state, id)),
			value_copy(val)
		)
	);
}

static struct r_res *
dereference_eval(struct ast_expr *, struct state *);

static struct r_res *
address_eval(struct ast_expr *, struct state *);

static struct r_res *
bang_eval(struct ast_expr *, struct state *);

static struct r_res *
expr_unary_eval(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_unary_op(expr)) {
	case UNARY_OP_DEREFERENCE:
		return dereference_eval(expr, state);
	case UNARY_OP_ADDRESS:
		return address_eval(expr, state);
	case UNARY_OP_BANG:
		return bang_eval(expr, state);
	default:
		assert(false);
	}
}

static struct r_res *
dereference_eval(struct ast_expr *expr, struct state *state)
{
	struct l_res *l_res = ast_expr_lvalue(expr, state);
	if (l_res_iserror(l_res)) {
		return r_res_error_create(l_res_as_error(l_res));
	}
	struct lvalue *lval = l_res_as_lvalue(l_res);

	struct ast_type *t = ast_type_copy(lvalue_type(lval));

	struct object *obj = lvalue_object(lval);
	if (!object_hasvalue(obj)) {
		char *s = ast_expr_str(expr);
		struct error *e = error_printf(
			"undefined indirection: %s has no value", s
		);
		free(s);
		return r_res_error_create(e);
	}
	struct value *v = value_copy(object_as_value(obj));

	l_res_destroy(l_res);

	return r_res_rvalue_create(rvalue_create(t, v));
}

static struct r_res *
address_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *operand = ast_expr_unary_operand(expr);
	char *id = ast_expr_as_identifier(operand);
	struct value *v = state_getloc(state, id);
	struct ast_type *t = state_getvariabletype(state, id);
	return r_res_rvalue_create(rvalue_create(ast_type_copy(t), v));
}

static struct r_res *
bang_eval(struct ast_expr *expr, struct state *state)
{
	struct r_res *res = ast_expr_eval(ast_expr_unary_operand(expr), state);
	if (r_res_iserror(res)) {
		return res;
	}
	struct rvalue *rval = r_res_as_rvalue(res);
	return r_res_rvalue_create(
		rvalue_create(rvalue_type(rval), value_bang(rvalue_value(rval)))
	);
}

static struct r_res *
expr_structmember_eval(struct ast_expr *expr, struct state *s)
{
	struct ast_expr *root = ast_expr_member_root(expr);
	struct r_res *res = ast_expr_eval(root, s);
	if (r_res_iserror(res)) {
		return res;
	}
	struct value *root_val = rvalue_value(r_res_as_rvalue(res));
	char *field = ast_expr_member_field(expr);
	struct object *member = value_struct_member(root_val, field);
	if (!member) {
		char *root_str = ast_expr_str(root);
		struct error *e = error_printf(
			"`%s' has no field `%s'", root_str, field
		);
		free(root_str);
		return r_res_error_create(e);
	}
	struct value *obj_value = object_as_value(member);
	/* XXX */
	struct value *v = obj_value ? value_copy(obj_value) : NULL;
	struct ast_type *t = ast_type_copy(
		value_struct_membertype(root_val, field)
	);
	r_res_destroy(res);
	return r_res_rvalue_create(rvalue_create(t, v));
}

/* expr_call_eval */

struct r_res_arr *
r_res_arr_create()
{
	return calloc(1, sizeof(struct r_res_arr));
}

void
r_res_arr_destroy(struct r_res_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		r_res_destroy(arr->res[i]);
	}
	free(arr);
}

void
r_res_arr_append(struct r_res_arr *arr, struct r_res *res)
{
	arr->res = realloc(arr->res, sizeof(struct r_res *) * ++arr->n);
	arr->res[arr->n-1] = res;
}

struct r_res_arr *
prepare_arguments(int nargs, struct ast_expr **arg, int nparams,
		struct ast_variable **param, struct state *state);

struct error *
prepare_parameters(int nparams, struct ast_variable **param, 
		struct r_res_arr *args, char *fname, struct state *state);

static struct error *
call_setupverify(struct ast_function *, struct ast_expr *, struct state *state);

static struct r_res *
expr_call_eval(struct ast_expr *expr, struct state *state)
{
	struct error *err;

	struct ast_expr *root = ast_expr_call_root(expr);
	/* TODO: function-valued-expressions */
	char *name = ast_expr_as_identifier(root);

	struct ast_function *f = externals_getfunc(state_getext(state), name);
	if (!f) {
		return r_res_error_create(error_printf("`%s' not found\n", name));
	}

	int nparams = ast_function_nparams(f);
	struct ast_variable **params = ast_function_params(f);

	int nargs = ast_expr_call_nargs(expr);
	if (nargs != nparams) {
		return r_res_error_create(
			error_printf(
				"`%s' given %d arguments instead of %d\n",
				name, nargs, nparams
			)
		);
	}

	struct r_res_arr *args = prepare_arguments(
		nargs, ast_expr_call_args(expr),
		nparams, params,
		state
	);

	struct frame *call_frame = frame_call_create(
		ast_function_name(f),
		ast_function_abstract(f),
		ast_function_type(f),
		state_next_execmode(state),
		ast_expr_copy(expr),
		f
	);
	state_pushframe(state, call_frame);

	if ((err = prepare_parameters(nparams, params, args, name, state))) {
		return r_res_error_create(err);
	}

	/* XXX: pass copy so we don't observe */
	if ((err = call_setupverify(f, ast_expr_copy(expr), state_copy(state)))) {
		return r_res_error_create(
			error_printf("precondition failure: %w", err)
		);
	}

	return r_res_rvalue_create(NULL);
}

static struct error *
verify_paramspec(struct value *param, struct value *arg, struct state *param_state,
		struct state *arg_state);

static struct error *
call_setupverify(struct ast_function *f, struct ast_expr *call, struct state *arg_state)
{
	struct error *err;

	char *fname = ast_function_name(f);
	struct frame *setupframe = frame_call_create(
		fname,
		ast_function_abstract(f),
		ast_function_type(f),
		EXEC_ABSTRACT_NO_SETUP,
		ast_expr_copy(call),
		f
	);
	struct state *param_state = state_create(
		setupframe,
		state_getext(arg_state)
	);
	if ((err = ast_function_initparams(f, param_state))) {
		return err;
	}
	if ((err = ast_function_initsetup(f, param_state))) {
		assert(false);
	}

	int nparams = ast_function_nparams(f);
	struct ast_variable **param = ast_function_params(f);

	for (int i = 0; i < nparams; i++) {
		char *id = ast_variable_name(param[i]);
		struct value *param = state_getloc(param_state, id);
		struct value *arg = state_getloc(arg_state, id);
		if ((err = verify_paramspec(param, arg, param_state, arg_state))) {
			return error_printf(
				"parameter `%s' of `%s' %w", id, fname, err
			);
		}
	}
	return NULL;
}

static struct error *
verify_paramspec(struct value *param, struct value *arg, struct state *param_state,
		struct state *arg_state)
{
	if (!state_islval(param_state, param)) {
		return NULL;
	}
	if (!state_islval(arg_state, arg)) {
		return error_printf("must be lvalue");
	}
	if (state_isalloc(param_state, param) && !state_isalloc(arg_state, arg)) {
		return error_printf("must be heap allocated");
	}
	struct object_res *param_res = state_get(
		param_state, value_as_location(param), false
	);
	if (object_res_iserror(param_res)) {
		return object_res_as_error(param_res);
	}
	struct object_res *arg_res = state_get(
		arg_state, value_as_location(arg), false
	);
	if (object_res_iserror(arg_res)) {
		return object_res_as_error(arg_res);
	}
	assert(object_res_hasobject(param_res));
	assert(object_res_hasobject(arg_res));
	struct object *param_obj = object_res_as_object(param_res),
		      *arg_obj = object_res_as_object(arg_res);
	if (!object_hasvalue(param_obj)) {
		return NULL; /* spec makes no claim about param */
	}
	if (!object_hasvalue(arg_obj)) {
		return error_printf("must be rvalue");
	}
	return verify_paramspec(
		object_as_value(param_obj),
		object_as_value(arg_obj),
		param_state, arg_state
	);
}

static struct ast_type *
calloralloc_type(struct ast_expr *, struct state *s);

struct r_res *
ast_expr_pf_augment(struct value *v, struct ast_expr *expr,
		struct state *state)
{
	struct ast_type *t = calloralloc_type(expr, state);
	if (!value_isstruct(v)) {
		return r_res_rvalue_create(
			rvalue_create(ast_type_copy(t), value_copy(v))
		);
	}
	struct r_res *res = ast_expr_pf_reduce(expr, state);
	if (r_res_iserror(res)) {
		return res;
	}
	struct rvalue *rval = r_res_as_rvalue(res);
	return r_res_rvalue_create(
		rvalue_create(
			rvalue_type(rval),
			value_pf_augment(v, value_as_sync(rvalue_value(rval)))
		)
	);
}

static struct ast_type *
call_type(struct ast_expr *call, struct state *);

static struct ast_type *
calloralloc_type(struct ast_expr *e, struct state *s)
{
	switch (ast_expr_kind(e)) {
	case EXPR_CALL:
		return call_type(e, s);
	case EXPR_ALLOCATION:
		assert(ast_expr_alloc_kind(e) != DEALLOC);
		return ast_type_create_voidptr();
	default:
		assert(false);
	}
}

static struct ast_type *
call_type(struct ast_expr *call, struct state *state)
{
	struct ast_expr *root = ast_expr_call_root(call);
	char *name = ast_expr_as_identifier(root);
	struct ast_function *f = externals_getfunc(state_getext(state), name);
	assert(f);
	return ast_type_copy(ast_function_type(f));
}


static struct r_res *
call_to_computed_value(struct ast_function *, struct state *s);

struct value *
ast_expr_call_arbitrary(struct ast_expr *expr, struct ast_function *f,
		struct state *state)
{
	struct r_res *res = call_to_computed_value(f, state);
	if (r_res_iserror(res)) {
		assert(false);
	}
	assert(r_res_hasrvalue(res));
	return rvalue_value(r_res_as_rvalue(res));
}

static struct r_res *
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
		struct r_res *res = ast_expr_eval(param, s);
		ast_expr_destroy(param);
		if (r_res_iserror(res)) {
			return res;
		}
		assert(r_res_hasrvalue(res));
		struct value *v = rvalue_value(r_res_as_rvalue(res));
		if (value_islocation(v)) {
			computed_param[i] = ast_expr_identifier_create(value_str(v));
		} else {
			computed_param[i] = value_to_expr(v);
		}
	}

	return r_res_rvalue_create(
		rvalue_create(
			ast_type_copy(ast_function_type(f)),
			value_sync_create(
				ast_expr_call_create(
					ast_expr_identifier_create(dynamic_str(root)),
					nparams, computed_param
				)
			)
		)
	);
}

struct r_res_arr *
prepare_arguments(int nargs, struct ast_expr **arg, int nparams,
		struct ast_variable **param, struct state *state)
{
	assert(nargs == nparams);

	struct r_res_arr *args = r_res_arr_create();
	for (int i = 0; i < nargs; i++) {
		r_res_arr_append(args, ast_expr_eval(arg[i], state));
	}
	return args;
}

/* prepare_parameters: Allocate arguments in call expression and assign them to
 * their respective parameters. */
struct error *
prepare_parameters(int nparams, struct ast_variable **param, 
		struct r_res_arr *args, char *fname, struct state *state)
{
	assert(nparams == args->n);

	for (int i = 0; i < args->n; i++) {
		state_declare(state, param[i], true);

		struct r_res *res = args->res[i];
		if (r_res_iserror(res)) {
			return r_res_as_error(res);
		}

		if (!r_res_hasrvalue(res)) {
			return error_printf(
				"parameter `%s' of `%s' has no value",
				ast_variable_name(param[i]), fname
			);
		}

		struct ast_expr *name = ast_expr_identifier_create(
			dynamic_str(ast_variable_name(param[i]))
		);
		struct l_res *lval_res = ast_expr_lvalue(name, state);
		if (l_res_iserror(lval_res)) {
			return l_res_as_error(lval_res);
		}
		struct object *obj = lvalue_object(l_res_as_lvalue(lval_res));
		ast_expr_destroy(name);

		struct value *v = rvalue_value(r_res_as_rvalue(res));
		object_assign(obj, value_copy(v));
	}
	return NULL;
}

static struct r_res *
expr_assign_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *lval = ast_expr_assignment_lval(expr),
			*rval = ast_expr_assignment_rval(expr);

	struct r_res *res = ast_expr_eval(rval, state);
	if (r_res_iserror(res)) {
		return res;
	}
	if (!r_res_hasrvalue(res)) {
		assert(false);
		return r_res_error_create(error_printf("undefined indirection (rvalue)"));
	}
	struct l_res *lval_res = ast_expr_lvalue(lval, state);
	if (l_res_iserror(lval_res)) {
		return r_res_error_create(l_res_as_error(lval_res));
	}
	struct object *obj = lvalue_object(l_res_as_lvalue(lval_res));
	if (!obj) {
		char *s = ast_expr_str(lval);
		struct error *e = error_printf(
			"undefined indirection: %s is not an lvalue", s
		);
		free(s);
		return r_res_error_create(e);
	}

	struct value *v = rvalue_value(r_res_as_rvalue(res));
	object_assign(obj, value_copy(v));

	return res;
}

static struct r_res *
expr_incdec_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *assign = ast_expr_incdec_to_assignment(expr);

	struct r_res *res;

	if (ast_expr_incdec_pre(expr)) { /* ++i */
		res = expr_assign_eval(assign, state);
	} else { /* i++ */
		res = ast_expr_eval(ast_expr_incdec_root(expr), state);
		/* assign and ignore result */ 
		r_res_destroy(expr_assign_eval(assign, state));
	}

	ast_expr_destroy(assign);

	return res;
}

static struct r_res *
value_binary_eval(struct rvalue *, enum ast_binary_operator, struct rvalue *);

static struct r_res *
expr_binary_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *e1 = ast_expr_binary_e1(expr),
			*e2 = ast_expr_binary_e2(expr);
	struct r_res *res1 = ast_expr_eval(e1, state),
		     *res2 = ast_expr_eval(e2, state);
	if (r_res_iserror(res1)) {
		return res1;
	}
	if (r_res_iserror(res2)) {
		return res2;
	}
	return value_binary_eval(
		r_res_as_rvalue(res1),
		ast_expr_binary_op(expr),
		r_res_as_rvalue(res2)
	);
}

static struct r_res *
value_binary_eval(struct rvalue *rv1, enum ast_binary_operator op,
		struct rvalue *rv2)
{
	struct value *v1 = rvalue_value(rv1),
		     *v2 = rvalue_value(rv2);
	if (value_islocation(v2)) {
		if (value_islocation(v1)) {
			a_printf(false, "adding two pointers not supported\n");
		}
		return value_binary_eval(rv2, op, rv1);
	}
	assert(value_isint(v2));
	struct ast_type *t1 = rvalue_type(rv1);
	/* ⊢ !value_islocation(v2) */
	if (value_islocation(v1)) {
		switch (op) {
		case BINARY_OP_ADDITION:
		case BINARY_OP_SUBTRACTION:
			break;
		default:
			assert(false);
		}

		struct location *loc1 = value_as_location(v1);
		struct location *newloc = location_copy(loc1);
		location_setoffset(
			newloc,
			ast_expr_binary_create(
				location_offset(loc1),
				op,
				/* v2 * sizeof(*v1) */
				ast_expr_product_create(
					value_to_expr(v2),
					ast_expr_constant_create(
						/* scaled by size of object
						 * pointed at */
						ast_type_size(ast_type_ptr_type(t1))
					)
				)
			)
		);
		struct value *v = value_ptr_create(newloc);
		return r_res_rvalue_create(rvalue_create(t1, v));
	}
	/* TODO: check t1, t2 are compatible */
	struct value *v = value_sync_create(
		ast_expr_binary_create(
			value_to_expr(v1), op, value_to_expr(v2)
		)
	);
	return r_res_rvalue_create(rvalue_create(t1, v));
}

static struct r_res *
arbarg_eval(struct ast_expr *expr, struct state *state)
{
	return r_res_rvalue_create(
		rvalue_create(
			/* XXX: we will investigate type conversions later */
			ast_type_create_range(NULL, NULL),
			state_vconst(
				state,
				/* XXX: we will investigate type conversions later */
				ast_type_create_ptr(ast_type_create(TYPE_VOID, 0)),
				NULL,
				false
			)
		)
	);
}

static struct r_res *
assign_absexec(struct ast_expr *, struct state *);

static struct r_res *
isdereferencable_absexec(struct ast_expr *, struct state *);

static struct r_res *
call_absexec(struct ast_expr *, struct state *);

static struct r_res *
alloc_absexec(struct ast_expr *, struct state *);

struct r_res *
ast_expr_abseval(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_ASSIGNMENT:
		return assign_absexec(expr, state);
	case EXPR_ISDEREFERENCABLE:
		return isdereferencable_absexec(expr, state);
	case EXPR_ALLOCATION:
		return alloc_absexec(expr, state);
	case EXPR_CALL:
		return call_absexec(expr, state);
	case EXPR_IDENTIFIER:
	case EXPR_CONSTANT:
	case EXPR_UNARY:
	case EXPR_STRUCTMEMBER:
	case EXPR_ARBARG:
		return ast_expr_eval(expr, state);	
	default:
		assert(false);
	}
}

static struct r_res *
call_absexec(struct ast_expr *expr, struct state *state)
{
	breakpoint_reset();

	struct error *err;

	struct ast_expr *root = ast_expr_call_root(expr);
	/* TODO: function-valued-expressions */
	char *name = ast_expr_as_identifier(root);

	struct ast_function *f = externals_getfunc(state_getext(state), name);
	if (!f) {
		return r_res_error_create(error_printf("`%s' not found\n", name));
	}

	int nparams = ast_function_nparams(f);
	struct ast_variable **params = ast_function_params(f);

	int nargs = ast_expr_call_nargs(expr);
	if (nargs != nparams) {
		return r_res_error_create(
			error_printf(
				"`%s' given %d arguments instead of %d\n",
				name, nargs, nparams
			)
		);
	}

	struct r_res_arr *args = prepare_arguments(
		nargs, ast_expr_call_args(expr),
		nparams, params,
		state
	);

	struct frame *call_frame = frame_call_create(
		ast_function_name(f),
		ast_function_abstract(f),
		ast_function_type(f),
		state_next_execmode(state),
		ast_expr_copy(expr),
		f
	);
	state_pushframe(state, call_frame);

	if ((err = prepare_parameters(nparams, params, args, name, state))) {
		return r_res_error_create(err);
	}

	/* XXX: pass copy so we don't observe */
	if ((err = call_setupverify(f, ast_expr_copy(expr), state_copy(state)))) {
		return r_res_error_create(
			error_printf("precondition failure: %w", err)
		);
	}

	return r_res_rvalue_create(NULL);
}

static int
hack_constorone(struct ast_expr *, struct state *);

static struct r_res *
dealloc_process(struct ast_expr *, struct state *);

/* operates at location level. it either creates an object on the heap and
 * returns a location or gets the location pointed to by an lvalue and attempts
 * to free possibly returning an error */
static struct r_res *
alloc_absexec(struct ast_expr *expr, struct state *state)
{
	struct r_res *res;
	switch (ast_expr_alloc_kind(expr)) {
	case ALLOC:
		/* TODO: size needs to be passed in here when added to .alloc */
		res = r_res_rvalue_create(
			rvalue_create(
				ast_type_create_void(),
				value_ptr_create(
					state_alloc(
						state,
						hack_constorone(
							ast_expr_alloc_arg(expr),
							state
						)
					)
				)
			)
		);
		break;
	case DEALLOC:
		res = dealloc_process(expr, state);
		break;
	case CLUMP:
		res = r_res_rvalue_create(
			rvalue_create(
				ast_type_create_void(),
				state_clump(state)
			)
		);
		break;
	default:
		assert(false);
	}
	if (r_res_iserror(res)) {
		return res;
	}
	if (r_res_hasrvalue(res)) {
		state_writeregister(
			state, rvalue_value(r_res_as_rvalue(res))
		);
	}
	return res;
}

static int
hack_constorone(struct ast_expr *e, struct state *s)
{
	if (ast_expr_isconstant(e)) {
		return ast_expr_isconstant(e) ? ast_expr_as_constant(e) : 1;
	}
	struct object_res *obj_res = state_getobject(s, ast_expr_as_identifier(e));
	if (object_res_iserror(obj_res)) {
		assert(false);
	}
	struct value *v = object_as_value(object_res_as_object(obj_res));
	return value_isconstant(v) ? value_as_constant(v) : 1;
}

static struct r_res *
dealloc_process(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *arg = ast_expr_alloc_arg(expr);
	/* arg is pointing at the heap location we want to free, so we want its
	 * value rather than location */
	struct r_res *res = ast_expr_eval(arg, state);
	if (r_res_iserror(res)) {
		return res;
	}
	struct value *val = rvalue_value(r_res_as_rvalue(res));
	assert(val);
	if (!value_islocation(val)) {
		return r_res_error_create(
			error_printf("undefined free of value not pointing at heap")
		);
	}
	struct error *err = state_dealloc(state, value_as_location(val));
	if (err) {
		return r_res_error_create(err);
	}
	value_destroy(val);
	return r_res_rvalue_create(NULL);
}

static struct r_res *
assign_absexec(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *lval = ast_expr_assignment_lval(expr),
			*rval = ast_expr_assignment_rval(expr);

	struct r_res *res = ast_expr_abseval(rval, state);
	if (r_res_iserror(res)) {
		return res;
	}
	if (!r_res_hasrvalue(res)) {
		assert(false);
		return r_res_error_create(error_printf("undefined indirection (rvalue)"));
	}
	struct l_res *lval_res = ast_expr_lvalue(lval, state);
	if (l_res_iserror(lval_res)) {
		return r_res_error_create(l_res_as_error(lval_res));
	}
	struct object *obj = lvalue_object(l_res_as_lvalue(lval_res));
	if (!obj) {
		return r_res_error_create(error_printf("undefined indirection (lvalue)"));
	}

	struct value *v = rvalue_value(r_res_as_rvalue(res));
	object_assign(obj, value_copy(v));

	return res;
}

static struct r_res *
isdereferencable_absexec(struct ast_expr *expr, struct state *state)
{
	struct props *p = state_getprops(state);
	props_install(p, expr);
	return r_res_rvalue_create(NULL);
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
	struct r_res *res = ast_expr_eval(expr, s_copy);

	/* TODO: user errors */
	assert(!r_res_iserror(res) && r_res_hasrvalue(res));

	state_destroy(s_copy);

	struct value *res_v = rvalue_value(r_res_as_rvalue(res));
	return irreducible_assume(value_as_sync(res_v), value, s);
}

static struct preresult *
ast_expr_pf_reduce_assume(struct ast_expr *expr, bool value, struct state *s)
{
	struct r_res *res = ast_expr_pf_reduce(expr, s);
	/* TODO: user errors */
	assert(!r_res_iserror(res) && r_res_hasrvalue(res));

	struct value *res_v = rvalue_value(r_res_as_rvalue(res));
	return irreducible_assume(value_as_sync(res_v), value, s);
}

static struct r_res *
binary_pf_reduce(struct ast_expr *e1, enum ast_binary_operator,
		struct ast_expr *e2, struct state *);

static struct r_res *
unary_pf_reduce(struct ast_expr *, struct state *);

static struct r_res *
call_pf_reduce(struct ast_expr *, struct state *);

static struct r_res *
structmember_pf_reduce(struct ast_expr *, struct state *);

struct r_res *
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
	case EXPR_BRACKETED:
		return ast_expr_pf_reduce(ast_expr_bracketed_root(e), s);
	default:
		assert(false);
	}
}

static struct r_res *
unary_pf_reduce(struct ast_expr *e, struct state *s)
{
	/* TODO: reduce by actually dereferencing if expr is a deref and this is
	 * possible in the current state */
	struct r_res *res = ast_expr_pf_reduce(ast_expr_unary_operand(e), s);
	if (r_res_iserror(res)) {
		return res;
	}
	struct rvalue *rval = r_res_as_rvalue(res);
	return r_res_rvalue_create(
		rvalue_create(
			rvalue_type(rval),
			value_sync_create(
				ast_expr_unary_create(
					value_as_sync(rvalue_value(rval)),
					ast_expr_unary_op(e)
				)
			)
		)
	);
}

static struct r_res *
binary_pf_reduce(struct ast_expr *e1, enum ast_binary_operator op,
		struct ast_expr *e2, struct state *s)
{
	struct r_res *res1 = ast_expr_pf_reduce(e1, s);
	if (r_res_iserror(res1)) {
		return res1;
	}
	assert(r_res_hasrvalue(res1));
	struct r_res *res2 = ast_expr_pf_reduce(e2, s);
	if (r_res_iserror(res2)) {
		return res2;
	}
	assert(r_res_hasrvalue(res2));
	struct rvalue *rv1 = r_res_as_rvalue(res1),
		      *rv2 = r_res_as_rvalue(res2);
	return r_res_rvalue_create(
		rvalue_create(
			rvalue_type(rv1), /* TODO: compare types */
			value_sync_create(
				ast_expr_binary_create(
					value_to_expr(rvalue_value(rv1)),
					op,
					value_to_expr(rvalue_value(rv2))
				)
			)
		)
	);
}

static struct r_res *
call_pf_reduce(struct ast_expr *e, struct state *s)
{
	/* TODO: allow for exprs as root */
	char *name = ast_expr_as_identifier(ast_expr_call_root(e));

	struct ast_function *f = externals_getfunc(state_getext(s), name);
	if (!f) {
		return r_res_error_create(error_printf("`%s' not found\n", name));
	}

	int nargs = ast_expr_call_nargs(e);
	struct ast_expr **unreduced_arg = ast_expr_call_args(e);
	struct ast_expr **reduced_arg = malloc(sizeof(struct ast_expr *) *nargs);
	for (int i = 0; i < nargs; i++) {
		struct r_res *res = ast_expr_pf_reduce(unreduced_arg[i], s);
		if (r_res_iserror(res)) {
			return res;
		}
		assert(r_res_hasrvalue(res));
		struct value *v = rvalue_value(r_res_as_rvalue(res));
		reduced_arg[i] = ast_expr_copy(value_to_expr(v));
	}
	struct value *v = value_sync_create(
		ast_expr_call_create(
			ast_expr_identifier_create(dynamic_str(name)),
			nargs, reduced_arg
		)
	);
	return r_res_rvalue_create(
		rvalue_create(ast_type_copy(ast_function_type(f)), v)
	);
}

static struct r_res *
structmember_pf_reduce(struct ast_expr *expr, struct state *s)
{
	struct r_res *res = ast_expr_pf_reduce(ast_expr_member_root(expr), s);
	if (r_res_iserror(res)) {
		return res;
	}
	assert(r_res_hasrvalue(res));
	char *field = ast_expr_member_field(expr);
	struct rvalue *root_rval = r_res_as_rvalue(res);
	struct value *v = rvalue_value(root_rval);
	if (value_isstruct(v)) {
		struct object *obj = value_struct_member(v, field);
		struct value *obj_value = object_as_value(obj);
		struct ast_type *obj_type = value_struct_membertype(v, field);
		assert(obj_value && obj_type);
		return r_res_rvalue_create(
			rvalue_create(
				ast_type_copy(obj_type),
				value_copy(obj_value)
			)
		);
	}
	assert(value_issync(v));
	struct ast_type *t = ast_type_struct_membertype(
		rvalue_type(root_rval), field, state_getext(s)
	);
	return r_res_rvalue_create(
		rvalue_create(
			ast_type_copy(t),
			value_sync_create(
				ast_expr_member_create(
					value_as_sync(v),
					dynamic_str(field)
				)
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
	struct r_res *r1 = ast_expr_pf_reduce(expr->u.binary.e1, s),
		     *r2 = ast_expr_pf_reduce(expr->u.binary.e2, s);

	/* TODO: user errors */
	struct value *v1 = rvalue_value(r_res_as_rvalue(r1)),
		     *v2 = rvalue_value(r_res_as_rvalue(r2));

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

/*
given f(x)

want:
	<rtype> t0 = f(x);


given f(g(x), y);

want:
	<rtype g> t0 = g(x);
	<rtype f> t1 = f(t0, y);
 */

static struct ast_expr *
unary_geninstr(struct ast_expr *, struct lexememarker *, struct ast_block *,
		struct state *);

static struct ast_expr *
binary_geninstr(struct ast_expr *, struct lexememarker *, struct ast_block *,
		struct state *);

static struct ast_expr *
incdec_geninstr(struct ast_expr *, struct lexememarker *, struct ast_block *,
		struct state *);

static struct ast_expr *
alloc_geninstr(struct ast_expr *, struct lexememarker *, struct ast_block *,
		struct state *);

static struct ast_expr *
assign_geninstr(struct ast_expr *, struct lexememarker *, struct ast_block *,
		struct state *);

static struct ast_expr *
call_geninstr(struct ast_expr *, struct lexememarker *, struct ast_block *,
		struct state *);

static struct ast_expr *
structmember_geninstr(struct ast_expr *, struct lexememarker *, struct ast_block *,
		struct state *);

static struct ast_expr *
bracketed_geninstr(struct ast_expr *, struct lexememarker *, struct ast_block *,
		struct state *);

struct ast_expr *
ast_expr_geninstr(struct ast_expr *expr, struct lexememarker *loc,
		struct ast_block *b, struct state *s) 
{
	switch (ast_expr_kind(expr)) {
	case EXPR_CONSTANT:
	case EXPR_ISDEALLOCAND:
	case EXPR_IDENTIFIER:
	case EXPR_STRING_LITERAL:
	case EXPR_ARBARG:
		return expr;
	case EXPR_UNARY:
		return unary_geninstr(expr, loc, b, s);	
	case EXPR_BINARY:
		return binary_geninstr(expr, loc, b, s);
	case EXPR_INCDEC:
		return incdec_geninstr(expr, loc, b, s);
	case EXPR_ALLOCATION:
		return alloc_geninstr(expr, loc, b, s);	
	case EXPR_ASSIGNMENT:
		return assign_geninstr(expr, loc, b, s);
	case EXPR_CALL:
		return call_geninstr(expr, loc, b, s);
	case EXPR_STRUCTMEMBER:
		return structmember_geninstr(expr, loc, b, s);
	case EXPR_BRACKETED:
		return bracketed_geninstr(expr, loc, b, s);
	default:
		assert(false);
	}
}

static struct ast_expr *
unary_geninstr(struct ast_expr *expr, struct lexememarker *loc, struct ast_block *b,
		struct state *s)
{
	struct ast_expr *gen_operand = ast_expr_geninstr(
		ast_expr_unary_operand(expr), loc, b, s
	);
	return ast_expr_unary_create(gen_operand, ast_expr_unary_op(expr));
}

static struct ast_expr *
binary_geninstr(struct ast_expr *expr, struct lexememarker *loc, struct ast_block *b,
		struct state *s)
{
	struct ast_expr *gen_e1 = ast_expr_geninstr(ast_expr_binary_e1(expr), loc, b, s),
			*gen_e2 = ast_expr_geninstr(ast_expr_binary_e2(expr), loc, b, s);
	return ast_expr_binary_create(gen_e1, ast_expr_binary_op(expr), gen_e2);
}

static struct ast_expr *
incdec_geninstr(struct ast_expr *expr, struct lexememarker *loc, struct ast_block *b,
		struct state *s)
{
	struct ast_expr *gen_root = ast_expr_geninstr(
		ast_expr_incdec_root(expr), loc, b, s
	);
	return ast_expr_incdec_create(
		gen_root, ast_expr_incdec_inc(expr), ast_expr_incdec_pre(expr)
	);
}

static struct ast_expr *
alloc_geninstr(struct ast_expr *expr, struct lexememarker *loc, struct ast_block *b,
		struct state *s)
{
	struct ast_expr	*gen_arg = ast_expr_geninstr(ast_expr_alloc_arg(expr), loc, b, s);
	enum ast_alloc_kind kind = ast_expr_alloc_kind(expr);

	struct ast_expr *alloc = ast_expr_alloc_kind_create(gen_arg, kind);
	struct ast_type *rtype = kind == DEALLOC
		? ast_type_create_void()
		: ast_type_create_voidptr();
	return ast_block_call_create(b, loc, rtype, alloc);
}

static struct ast_expr *
assign_geninstr(struct ast_expr *expr, struct lexememarker *loc, struct ast_block *b,
		struct state *s)
{
	struct ast_expr *lval = ast_expr_assignment_lval(expr),
			*rval = ast_expr_assignment_rval(expr);
	assert(
		ast_expr_kind(lval) == EXPR_IDENTIFIER ||
		ast_expr_kind(lval) == EXPR_UNARY ||
		ast_expr_kind(lval) == EXPR_STRUCTMEMBER
	);
	struct ast_expr *gen_rval = ast_expr_geninstr(rval, loc, b, s);
	if (!gen_rval) {
		assert(false); /* XXX: user error void func */
	}
	struct ast_expr *assign = ast_expr_assignment_create(
		ast_expr_copy(lval), gen_rval
	);
	ast_block_append_stmt(b, ast_stmt_create_expr(loc, assign));
	return lval;
}

static struct ast_expr *
call_geninstr(struct ast_expr *expr, struct lexememarker *loc,
		struct ast_block *b, struct state *s)
{
	int nargs = ast_expr_call_nargs(expr);
	struct ast_expr **args = ast_expr_call_args(expr);

	struct ast_expr **gen_args = malloc(sizeof(struct ast_expr *) * nargs);

	for (int i = 0; i < nargs; i++) {
		struct ast_expr *gen_arg = ast_expr_geninstr(args[i], loc, b, s);
		if (!gen_arg) {
			assert(false); /* XXX: user error void func */
		}
		gen_args[i] = gen_arg;
	}

	struct ast_expr *root = ast_expr_call_root(expr);
	/* XXX: handle root thats a call */
	char *name = ast_expr_as_identifier(root);
	struct ast_function *f = externals_getfunc(state_getext(s), name);
	assert(f);
	struct ast_type *rtype = ast_function_type(f);
	struct ast_expr *call = ast_expr_call_create(
		ast_expr_copy(root), nargs, gen_args
	);
	return ast_block_call_create(b, loc, rtype, call);
}

static struct ast_expr *
structmember_geninstr(struct ast_expr *expr, struct lexememarker *loc, struct ast_block *b,
		struct state *s)
{
	struct ast_expr *root_gen = ast_expr_geninstr(
		ast_expr_member_root(expr), loc, b, s
	);
	if (!root_gen) {
		assert(false); /* XXX: user error void root */
	}
	return ast_expr_member_create(root_gen, dynamic_str(ast_expr_member_field(expr)));
}

static struct ast_expr *
bracketed_geninstr(struct ast_expr *expr, struct lexememarker *loc, struct ast_block *b,
		struct state *s)
{
	struct ast_expr *gen_root = ast_expr_geninstr(
		ast_expr_bracketed_root(expr), loc, b, s
	);
	return ast_expr_bracketed_create(gen_root);
}
