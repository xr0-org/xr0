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

	struct e_res *res = ast_expr_eval(ast_expr_binary_e1(acc), state);
	if (e_res_iserror(res)) {
		assert(false);
	}
	struct eval *e = e_res_as_eval(res);
	struct object_res *obj_res = state_get(state, eval_as_lval(e), true);
	struct object *obj = object_res_as_object(obj_res);
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
	
	struct e_res *e_res_lw = ast_expr_eval(lw, state),
		     *e_res_up = ast_expr_eval(up, state);
	if (e_res_iserror(e_res_lw)) {
		return e_res_as_error(e_res_lw);
	}
	if (e_res_iserror(e_res_up)) {
		return e_res_as_error(e_res_up);
	}
	struct eval *eval_lw = e_res_as_eval(e_res_lw),
		    *eval_up = e_res_as_eval(e_res_up);

	struct ast_expr *res_lw = value_to_expr(eval_as_rval(eval_lw)),
			*res_up = value_to_expr(eval_as_rval(eval_up));

	e_res_destroy(e_res_up);
	e_res_destroy(e_res_lw);

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
	struct e_res *res = ast_expr_eval(ast_expr_binary_e1(inner), state);
	if (e_res_iserror(res)) {
		assert(false);
	}
	return object_res_as_object(
		state_get(state, eval_as_lval(e_res_as_eval(res)), true)
	);
}

struct error *
ast_expr_exec(struct ast_expr *expr, struct state *state)
{
	struct e_res *res = ast_expr_eval(expr, state);
	if (e_res_iserror(res)) {
		return e_res_as_error(res);
	}
	/* e_res_destroy(res); */
	return NULL;
}

static struct object *
hack_object_from_assertion(struct ast_expr *expr, struct state *state)
{	
	/* get assertand */
	struct ast_expr *assertand = ast_expr_isdeallocand_assertand(expr);

	/* get `assertand' variable */
	struct e_res *res = ast_expr_eval(assertand, state);
	if (e_res_iserror(res)) {
		assert(false);
	}
	return object_res_as_object(
		state_get(state, eval_as_lval(e_res_as_eval(res)), true)
	);
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
	struct e_res *root = ast_expr_eval(ast_expr_binary_e1(expr), state),
		     *last = ast_expr_eval(ast_expr_binary_e2(expr), state);

	assert(!e_res_iserror(root) && !e_res_iserror(last));

	return value_compare(
		eval_as_rval(e_res_as_eval(root)),
		ast_expr_binary_op(expr),
		eval_as_rval(e_res_as_eval(last))
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

static struct e_res *
expr_constant_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_literal_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_identifier_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_unary_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_structmember_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_call_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_assign_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_incdec_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_binary_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
range_eval(struct ast_expr *expr, struct state *state);

struct e_res *
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
	case EXPR_RANGE:
		return range_eval(expr, state);
	case EXPR_BRACKETED:
		return ast_expr_eval(ast_expr_bracketed_root(expr), state);
	default:
		assert(false);
	}
}

static struct e_res *
expr_literal_eval(struct ast_expr *expr, struct state *state)
{
	return e_res_eval_create(
		eval_rval_create(
			ast_type_create_char(),
			state_static_init(state, expr)
		)
	);
}

static struct e_res *
expr_constant_eval(struct ast_expr *expr, struct state *state)
{
	return e_res_eval_create(
		eval_rval_create(
			ast_type_create_int(),
			value_int_create(ast_expr_as_constant(expr))
		)
	);
}

static struct e_res *
hack_identifier_builtin_eval(char *id, struct state *state);

static struct e_res *
expr_identifier_eval(struct ast_expr *expr, struct state *state)
{
	struct e_res *res = hack_identifier_builtin_eval(
		ast_expr_as_identifier(expr), state
	);
	if (!e_res_iserror(res) && e_res_haseval(res)) {
		return res;
	}

	char *id = ast_expr_as_identifier(expr);

	/* XXX */
	if (id[0] == '#') {
		assert(false);
		return e_res_eval_create(
			eval_rval_create(
				ast_type_create_char(),
				value_literal_create(id)
			)
		);
	}

	/* TODO: check that id exists in state return error if not */

	return e_res_eval_create(
		eval_lval_create(
			ast_type_copy(state_getvariabletype(state, id)),
			location_copy(state_getloc(state, id))
		)
	);
}

static struct e_res *
hack_identifier_builtin_eval(char *id, struct state *state)
{
	if (state_getrconst(state, id) || strncmp(id, "ptr:", 4) == 0) {
		/* TODO set type from rconsts */
		return e_res_eval_create(
			eval_rval_create(
				ast_type_create_voidptr(),
				value_rconst_create(
					ast_expr_identifier_create(dynamic_str(id))
				)
			)
		);
	}
	return e_res_error_create(error_printf("not built-in"));
}

static struct e_res *
dereference_eval(struct ast_expr *, struct state *);

static struct e_res *
address_eval(struct ast_expr *, struct state *);

static struct e_res *
bang_eval(struct ast_expr *, struct state *);

static struct e_res *
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

static struct e_res *
dereference_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *operand = ast_expr_unary_operand(expr);
	struct e_res *res = ast_expr_eval(operand, state);
	if (e_res_iserror(res)) {
		return e_res_error_create(e_res_as_error(res));
	}
	struct eval *eval = e_res_as_eval(res);

	struct ast_type *t = ast_type_copy(ast_type_ptr_type(eval_type(eval)));

	struct value_res *v_res = eval_to_value(eval, state);
	if (value_res_iserror(v_res)) {
		return e_res_error_create(value_res_as_error(v_res));
	}
	if (!value_res_hasvalue(v_res)) {
		char *s = ast_expr_str(operand);
		struct error *e = error_printf(
			"undefined indirection: %s has no value", s
		);
		free(s);
		return e_res_error_create(e);
	}
	struct value *v = value_res_as_value(v_res);
	if (!value_islocation(v)) {
		char *s = ast_expr_str(expr);
		struct error *e = error_printf(
			"undefined indirection: %s is %s not lvalue", s, value_type_str(v)
		);
		free(s);
		return e_res_error_create(e);
	}
	struct location *loc = location_copy(value_as_location(v));


	return e_res_eval_create(eval_lval_create(t, loc));
}

static struct e_res *
address_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *operand = ast_expr_unary_operand(expr);
	struct e_res *res = ast_expr_eval(operand, state);
	if (e_res_iserror(res)) {
		return res;
	}
	struct eval *eval = e_res_as_eval(res);
	struct value *v = value_ptr_create(location_copy(eval_as_lval(eval)));
	struct ast_type *t = ast_type_create_ptr(ast_type_copy(eval_type(eval)));
	e_res_destroy(res);
	return e_res_eval_create(eval_rval_create(ast_type_copy(t), v));
}

static struct e_res *
bang_eval(struct ast_expr *expr, struct state *state)
{
	struct e_res *res = ast_expr_eval(ast_expr_unary_operand(expr), state);
	if (e_res_iserror(res)) {
		return res;
	}
	struct eval *eval = e_res_as_eval(res);
	return e_res_eval_create(
		eval_rval_create(eval_type(eval), value_bang(eval_as_rval(eval)))
	);
}

static struct e_res *
expr_structmember_eval(struct ast_expr *expr, struct state *s)
{
	struct ast_expr *root_expr = ast_expr_member_root(expr);
	struct e_res *res = ast_expr_eval(root_expr, s);
	if (e_res_iserror(res)) {
		return res;
	}
	/* XXX: insists on an lvalue (in the sense of location) for root */
	struct eval *root_eval = e_res_as_eval(res);
	struct location *root_loc = eval_as_lval(root_eval);
	struct object *root_obj = object_res_as_object(
		state_get(s, root_loc, true)
	);
	char *field = ast_expr_member_field(expr);
	struct ast_type *t = ast_type_copy(
		object_getmembertype(
			root_obj, eval_type(root_eval), field, s
		)
	);
	assert(t);
	struct location *l = location_copy(root_loc);
	location_setoffset(
		l,
		offset_create_member(
			location_offset(l),
			dynamic_str(field),
			ast_type_copy(t)
		)
	);
	e_res_destroy(res);
	return e_res_eval_create(eval_lval_create(t, l));
}

/* expr_call_eval */

static struct error *
call_setupverify(struct ast_function *, struct ast_expr *, struct state *state);

static struct e_res *
expr_call_eval(struct ast_expr *expr, struct state *state)
{
	struct error *err;

	struct ast_expr *root = ast_expr_call_root(expr);
	/* TODO: function-valued-expressions */
	char *name = ast_expr_as_identifier(root);

	struct ast_function *f = externals_getfunc(state_getext(state), name);
	if (!f) {
		return e_res_error_create(error_printf("`%s' not found\n", name));
	}

	int nparams = ast_function_nparams(f);
	struct ast_variable **params = ast_function_params(f);

	int nargs = ast_expr_call_nargs(expr);
	if (nargs != nparams) {
		return e_res_error_create(
			error_printf(
				"`%s' given %d arguments instead of %d\n",
				name, nargs, nparams
			)
		);
	}

	struct value_arr_res *args_res = prepare_arguments(
		nargs, ast_expr_call_args(expr),
		nparams, params,
		state
	);
	if (value_arr_res_iserror(args_res)) {
		return e_res_error_create(value_arr_res_as_error(args_res));
	}
	struct value_arr *args = value_arr_res_as_arr(args_res);

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
		return e_res_error_create(err);
	}

	/* XXX: pass copy so we don't observe */
	if ((err = call_setupverify(f, ast_expr_copy(expr), state_copy(state)))) {
		return e_res_error_create(
			error_printf("precondition failure: %w", err)
		);
	}

	return e_res_eval_create(NULL);
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
		struct value *param = value_ptr_create(
			location_copy(state_getloc(param_state, id))
		);
		struct value *arg = value_ptr_create(
			location_copy(state_getloc(arg_state, id))
		);
		err = verify_paramspec(param, arg, param_state, arg_state);
		value_destroy(arg);
		value_destroy(param);
		if (err) {
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

static bool
shouldarrconv(struct e_res *);

static struct e_res *
ast_expr_evalarrconv(struct ast_expr *e, struct state *s)
{
	struct e_res *res = ast_expr_eval(e, s);
	if (!shouldarrconv(res)) {
		return res;
	}

	struct eval *eval = e_res_as_eval(res);
	struct value *v = value_ptr_create(location_copy(eval_as_lval(eval)));
	struct ast_type *t = ast_type_create_ptr(
		ast_type_copy(ast_type_arr_type(eval_type(eval)))
	);

	e_res_destroy(res);

	return e_res_eval_create(eval_rval_create(t, v));
}

static bool
shouldarrconv(struct e_res *res)
{
	return !e_res_iserror(res)
		&& e_res_haseval(res)
		&& ast_type_isarr(eval_type(e_res_as_eval(res)));
}

static struct ast_type *
calloralloc_type(struct ast_expr *, struct state *s);

struct e_res *
ast_expr_pf_augment(struct value *v, struct ast_expr *expr,
		struct state *state)
{
	struct ast_type *t = calloralloc_type(expr, state);
	if (!value_isstruct(v)) {
		return e_res_eval_create(
			eval_rval_create(ast_type_copy(t), value_copy(v))
		);
	}
	struct e_res *res = ast_expr_pf_reduce(expr, state);
	if (e_res_iserror(res)) {
		return res;
	}
	struct eval *eval = e_res_as_eval(res);
	return e_res_eval_create(
		eval_rval_create(
			eval_type(eval),
			value_pf_augment(v, value_as_rconst(eval_as_rval(eval)))
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


static struct e_res *
call_to_computed_value(struct ast_function *, struct state *s);

struct value *
ast_expr_call_arbitrary(struct ast_expr *expr, struct ast_function *f,
		struct state *state)
{
	struct e_res *res = call_to_computed_value(f, state);
	if (e_res_iserror(res)) {
		assert(false);
	}
	return eval_as_rval(e_res_as_eval(res));
}

static struct e_res *
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
		struct e_res *res = ast_expr_eval(param, s);
		ast_expr_destroy(param);
		if (e_res_iserror(res)) {
			return res;
		}
		struct value *v = value_res_as_value(
			eval_to_value(e_res_as_eval(res), s)
		);
		if (value_islocation(v)) {
			computed_param[i] = ast_expr_identifier_create(value_str(v));
		} else {
			computed_param[i] = value_to_expr(v);
		}
	}

	return e_res_eval_create(
		eval_rval_create(
			ast_type_copy(ast_function_type(f)),
			value_rconst_create(
				ast_expr_call_create(
					ast_expr_identifier_create(dynamic_str(root)),
					nparams, computed_param
				)
			)
		)
	);
}

struct value_arr_res *
prepare_arguments(int nargs, struct ast_expr **arg, int nparams,
		struct ast_variable **param, struct state *s)
{
	assert(nargs == nparams);

	struct value_arr *args = value_arr_create();
	for (int i = 0; i < nargs; i++) {
		struct e_res *res = ast_expr_eval(arg[i], s);
		if (e_res_iserror(res)) {
			return value_arr_res_error_create(e_res_as_error(res));
		}
		struct value_res *v_res = eval_to_value(e_res_as_eval(res), s);
		if (value_res_iserror(v_res)) {
			return value_arr_res_error_create(
				value_res_as_error(v_res)
			);
		}
		if (!value_res_hasvalue(v_res)) {
			char *expr = ast_expr_str(arg[i]);
			struct error *err = error_printf(
				"undefined memory access: %s has no value", expr
			);
			free(expr);
			return value_arr_res_error_create(err);
		}
		value_arr_append(args, value_res_as_value(v_res));
	}
	return value_arr_res_arr_create(args);
}

/* prepare_parameters: Allocate arguments in call expression and assign them to
 * their respective parameters. */
struct error *
prepare_parameters(int nparams, struct ast_variable **param, 
		struct value_arr *args_arr, char *fname, struct state *state)
{
	int nargs = value_arr_len(args_arr);
	struct value **arg = value_arr_v(args_arr);
	assert(nparams == nargs);

	for (int i = 0; i < nargs; i++) {
		state_declare(state, param[i], true);

		struct ast_expr *name = ast_expr_identifier_create(
			dynamic_str(ast_variable_name(param[i]))
		);
		struct e_res *lval_res = ast_expr_eval(name, state);
		if (e_res_iserror(lval_res)) {
			return e_res_as_error(lval_res);
		}
		struct object *l_obj = object_res_as_object(
			state_get(state, eval_as_lval(e_res_as_eval(lval_res)), true)
		);
		ast_expr_destroy(name);

		object_assign(l_obj, value_copy(arg[i]));
	}
	return NULL;
}

static struct e_res *
expr_assign_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *lval = ast_expr_assignment_lval(expr),
			*rval = ast_expr_assignment_rval(expr);

	struct e_res *l_res = ast_expr_eval(lval, state);
	if (e_res_iserror(l_res)) {
		return e_res_error_create(e_res_as_error(l_res));
	}
	printf("%s\n", state_str(state));
	printf("expr: %s\n", ast_expr_str(expr));
	printf("eval (lval): %s\n", eval_str(e_res_as_eval(l_res)));
	struct object_res *obj_res = eval_to_object(
		e_res_as_eval(l_res), state, true
	);
	if (object_res_iserror(obj_res)) {
		return e_res_error_create(object_res_as_error(obj_res));
	}
	if (!object_res_hasobject(obj_res)) {
		char *s = ast_expr_str(lval);
		struct error *e = error_printf(
			"undefined indirection: %s is not an lvalue", s
		);
		free(s);
		return e_res_error_create(e);
	}
	struct object *obj = object_res_as_object(obj_res);

	struct e_res *r_res = ast_expr_eval(rval, state);
	if (e_res_iserror(r_res)) {
		return r_res;
	}
	struct eval *eval = e_res_as_eval(r_res);
	struct value_res *v_res = eval_to_value(eval, state);
	if (value_res_iserror(v_res)) {
		return e_res_error_create(value_res_as_error(v_res));
	}
	if (!value_res_hasvalue(v_res)) {
		char *s = ast_expr_str(rval);
		struct error *e = error_printf(
			"undefined memory access: %s has no value", s
		);
		free(s);
		return e_res_error_create(e);
	}
	object_assign(obj, value_copy(value_res_as_value(v_res)));

	return r_res;
}

static struct e_res *
expr_incdec_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *assign = ast_expr_incdec_to_assignment(expr);

	struct e_res *res;

	if (ast_expr_incdec_pre(expr)) { /* ++i */
		res = expr_assign_eval(assign, state);
	} else { /* i++ */
		res = ast_expr_eval(ast_expr_incdec_root(expr), state);
		/* assign and ignore result */ 
		e_res_destroy(expr_assign_eval(assign, state));
	}

	ast_expr_destroy(assign);

	return res;
}

static struct e_res *
value_binary_eval(struct eval *, enum ast_binary_operator, struct eval *, struct state *);

static struct e_res *
expr_binary_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *e1 = ast_expr_binary_e1(expr),
			*e2 = ast_expr_binary_e2(expr);
	struct e_res *res1 = ast_expr_evalarrconv(e1, state),
		     *res2 = ast_expr_evalarrconv(e2, state);
	if (e_res_iserror(res1)) {
		return res1;
	}
	if (e_res_iserror(res2)) {
		return res2;
	}
	return value_binary_eval(
		e_res_as_eval(res1),
		ast_expr_binary_op(expr),
		e_res_as_eval(res2),
		state
	);
}

static struct e_res *
value_binary_eval(struct eval *rv1, enum ast_binary_operator op,
		struct eval *rv2, struct state *s)
{
	struct value *v1 = value_res_as_value(eval_to_value(rv1, s)),
		     *v2 = value_res_as_value(eval_to_value(rv2, s));
	if (value_islocation(v2)) {
		if (value_islocation(v1)) {
			a_printf(false, "adding two pointers not supported\n");
		}
		return value_binary_eval(rv2, op, rv1, s);
	}
	assert(value_isint(v2));
	struct ast_type *t1 = eval_type(rv1);
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
		struct ast_expr *op1 = offset_as_expr(location_offset(loc1)),
				*op2 = ast_expr_product_create(
			/* v2 * sizeof(*v1) */
			value_to_expr(v2),
			ast_expr_constant_create(
				/* scaled by size of object * pointed at */
				ast_type_size(ast_type_ptr_type(t1))
			)
		);
		location_setoffset(
			newloc,
			offset_create(ast_expr_binary_create(op1, op, op2))
		);
		struct value *v = value_ptr_create(newloc);
		return e_res_eval_create(eval_rval_create(t1, v));
	}
	/* TODO: check t1, t2 are compatible */
	struct value *v = value_rconst_create(
		ast_expr_binary_create(
			value_to_expr(v1), op, value_to_expr(v2)
		)
	);
	return e_res_eval_create(eval_rval_create(t1, v));
}

static struct e_res *
range_eval(struct ast_expr *expr, struct state *state)
{
	printf("%s\n", state_str(state));
	printf("range: %s\n", ast_expr_str(expr));
	assert(false);
	return e_res_eval_create(
		eval_rval_create(
			/* XXX: we will investigate type conversions later */
			ast_type_create_range(),
			state_rconst(
				state,
				ast_type_create_range(),
				NULL,
				false
			)
		)
	);
}

static struct e_res *
assign_absexec(struct ast_expr *, struct state *);

static struct e_res *
isdereferencable_absexec(struct ast_expr *, struct state *);

static struct e_res *
call_absexec(struct ast_expr *, struct state *);

static struct e_res *
alloc_absexec(struct ast_expr *, struct state *);

struct e_res *
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
	case EXPR_RANGE:
		return ast_expr_eval(expr, state);	
	default:
		assert(false);
	}
}

static struct e_res *
call_absexec(struct ast_expr *expr, struct state *state)
{
	breakpoint_reset();

	struct error *err;

	struct ast_expr *root = ast_expr_call_root(expr);
	/* TODO: function-valued-expressions */
	char *name = ast_expr_as_identifier(root);

	struct ast_function *f = externals_getfunc(state_getext(state), name);
	if (!f) {
		return e_res_error_create(error_printf("`%s' not found\n", name));
	}

	int nparams = ast_function_nparams(f);
	struct ast_variable **params = ast_function_params(f);

	int nargs = ast_expr_call_nargs(expr);
	if (nargs != nparams) {
		return e_res_error_create(
			error_printf(
				"`%s' given %d arguments instead of %d\n",
				name, nargs, nparams
			)
		);
	}

	struct value_arr_res *args_res = prepare_arguments(
		nargs, ast_expr_call_args(expr),
		nparams, params,
		state
	);
	if (value_arr_res_iserror(args_res)) {
		return e_res_error_create(value_arr_res_as_error(args_res));
	}
	struct value_arr *args = value_arr_res_as_arr(args_res);

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
		return e_res_error_create(err);
	}

	/* XXX: pass copy so we don't observe */
	if ((err = call_setupverify(f, ast_expr_copy(expr), state_copy(state)))) {
		return e_res_error_create(
			error_printf("precondition failure: %w", err)
		);
	}

	return e_res_empty_create();
}

static int
hack_constorone(struct ast_expr *, struct state *);

static struct e_res *
dealloc_process(struct ast_expr *, struct state *);

/* operates at location level. it either creates an object on the heap and
 * returns a location or gets the location pointed to by an lvalue and attempts
 * to free possibly returning an error */
static struct e_res *
alloc_absexec(struct ast_expr *expr, struct state *state)
{
	struct e_res *res;
	switch (ast_expr_alloc_kind(expr)) {
	case ALLOC:
		/* TODO: size needs to be passed in here when added to .alloc */
		res = e_res_eval_create(
			eval_rval_create(
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
		res = e_res_eval_create(
			eval_rval_create(
				ast_type_create_void(),
				state_clump(state)
			)
		);
		break;
	default:
		assert(false);
	}
	if (e_res_iserror(res)) {
		return res;
	}
	if (e_res_haseval(res)) {
		state_writeregister(state, eval_as_rval(e_res_as_eval(res)));
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

static struct e_res *
dealloc_process(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *arg = ast_expr_alloc_arg(expr);
	/* arg is pointing at the heap location we want to free, so we want its
	 * value rather than location */
	struct e_res *res = ast_expr_eval(arg, state);
	if (e_res_iserror(res)) {
		return res;
	}
	struct eval *e = e_res_as_eval(res);
	assert(e);
	if (!eval_islval(e)) {
		assert(false);	
	}
	struct location *arg_loc = eval_as_lval(e);
	struct object_res *obj_res = state_deref(state, value_ptr_create(arg_loc));
	if (object_res_iserror(obj_res)) {
		assert(false);
	}
	struct object *obj = object_res_as_object(obj_res);
	if (!object_isvalue(obj)) {
		assert(false);
	}
	struct value *v = object_as_value(obj);
	if (!value_islocation(v)) {
		return e_res_error_create(
			error_printf("undefined free of value not pointing at heap")
		);
	}
	struct location *loc = value_as_location(v);
	struct error *err = state_dealloc(state, loc);
	if (err) {
		return e_res_error_create(err);
	}
	return e_res_empty_create();
}

static struct e_res *
assign_absexec(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *lval = ast_expr_assignment_lval(expr),
			*rval = ast_expr_assignment_rval(expr);

	struct e_res *res = ast_expr_abseval(rval, state);
	if (e_res_iserror(res)) {
		return res;
	}
	if (!e_res_haseval(res)) {
		assert(false);
		return e_res_error_create(error_printf("undefined indirection (rvalue)"));
	}
	struct e_res *l_res = ast_expr_eval(lval, state);
	if (e_res_iserror(l_res)) {
		return e_res_error_create(e_res_as_error(l_res));
	}
	struct object *obj = object_res_as_object(
		state_get(state, eval_as_lval(e_res_as_eval(l_res)), true)
	);
	if (!obj) {
		return e_res_error_create(error_printf("undefined indirection (lvalue)"));
	}

	struct value *v = value_res_as_value(
		eval_to_value(e_res_as_eval(res), state)
	);
	object_assign(obj, value_copy(v));

	return res;
}

static struct e_res *
isdereferencable_absexec(struct ast_expr *expr, struct state *state)
{
	struct props *p = state_getprops(state);
	props_install(p, expr);
	return e_res_eval_create(NULL);
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
	struct e_res *res = ast_expr_eval(expr, s_copy);

	/* TODO: user errors */
	assert(!e_res_iserror(res) && e_res_haseval(res));

	state_destroy(s_copy);

	struct value *res_v = eval_as_rval(e_res_as_eval(res));
	return irreducible_assume(value_as_rconst(res_v), value, s);
}

static struct preresult *
ast_expr_pf_reduce_assume(struct ast_expr *expr, bool value, struct state *s)
{
	struct e_res *res = ast_expr_pf_reduce(expr, s);
	/* TODO: user errors */
	assert(!e_res_iserror(res) && e_res_haseval(res));

	struct value *res_v = eval_as_rval(e_res_as_eval(res));
	return irreducible_assume(value_as_rconst(res_v), value, s);
}

static struct e_res *
binary_pf_reduce(struct ast_expr *e1, enum ast_binary_operator,
		struct ast_expr *e2, struct state *);

static struct e_res *
unary_pf_reduce(struct ast_expr *, struct state *);

static struct e_res *
call_pf_reduce(struct ast_expr *, struct state *);

static struct e_res *
structmember_pf_reduce(struct ast_expr *, struct state *);

struct e_res *
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

static struct e_res *
unary_pf_reduce(struct ast_expr *e, struct state *s)
{
	/* TODO: reduce by actually dereferencing if expr is a deref and this is
	 * possible in the current state */
	struct e_res *res = ast_expr_pf_reduce(ast_expr_unary_operand(e), s);
	if (e_res_iserror(res)) {
		return res;
	}
	struct eval *eval = e_res_as_eval(res);
	struct value *v = value_res_as_value(eval_to_value(eval, s));
	return e_res_eval_create(
		eval_rval_create(
			eval_type(eval),
			value_rconst_create(
				ast_expr_unary_create(
					value_as_rconst(v),
					ast_expr_unary_op(e)
				)
			)
		)
	);
}

static struct e_res *
binary_pf_reduce(struct ast_expr *e1, enum ast_binary_operator op,
		struct ast_expr *e2, struct state *s)
{
	struct e_res *res1 = ast_expr_pf_reduce(e1, s);
	if (e_res_iserror(res1)) {
		return res1;
	}
	assert(e_res_haseval(res1));
	struct e_res *res2 = ast_expr_pf_reduce(e2, s);
	if (e_res_iserror(res2)) {
		return res2;
	}
	assert(e_res_haseval(res2));
	struct eval *rv1 = e_res_as_eval(res1),
		    *rv2 = e_res_as_eval(res2);
	struct value *v1 = value_res_as_value(eval_to_value(rv1, s)),
		     *v2 = value_res_as_value(eval_to_value(rv2, s));
	return e_res_eval_create(
		eval_rval_create(
			eval_type(rv1), /* TODO: compare types */
			value_rconst_create(
				ast_expr_binary_create(
					value_to_expr(v1),
					op,
					value_to_expr(v2)
				)
			)
		)
	);
}

static struct e_res *
call_pf_reduce(struct ast_expr *e, struct state *s)
{
	/* TODO: allow for exprs as root */
	char *name = ast_expr_as_identifier(ast_expr_call_root(e));

	struct ast_function *f = externals_getfunc(state_getext(s), name);
	if (!f) {
		return e_res_error_create(error_printf("`%s' not found\n", name));
	}

	int nargs = ast_expr_call_nargs(e);
	struct ast_expr **unreduced_arg = ast_expr_call_args(e);
	struct ast_expr **reduced_arg = malloc(sizeof(struct ast_expr *) *nargs);
	for (int i = 0; i < nargs; i++) {
		struct e_res *res = ast_expr_pf_reduce(unreduced_arg[i], s);
		if (e_res_iserror(res)) {
			return res;
		}
		assert(e_res_haseval(res));
		struct value *v = value_res_as_value(
			eval_to_value(e_res_as_eval(res), s)
		);
		reduced_arg[i] = ast_expr_copy(value_to_expr(v));
	}
	struct value *v = value_rconst_create(
		ast_expr_call_create(
			ast_expr_identifier_create(dynamic_str(name)),
			nargs, reduced_arg
		)
	);
	return e_res_eval_create(
		eval_rval_create(ast_type_copy(ast_function_type(f)), v)
	);
}

static struct e_res *
structmember_pf_reduce(struct ast_expr *expr, struct state *s)
{
	struct e_res *res = ast_expr_pf_reduce(ast_expr_member_root(expr), s);
	if (e_res_iserror(res)) {
		return res;
	}
	/* XXX: insists on an lvalue (in the sense of location) for root */
	struct eval *eval = e_res_as_eval(res);
	struct value *v = value_res_as_value(eval_to_value(eval, s));
	char *field = ast_expr_member_field(expr);
	if (value_isstruct(v)) {
		struct object *obj = value_struct_member(v, field);
		struct value *obj_value = object_as_value(obj);
		struct ast_type *obj_type = value_struct_membertype(v, field);
		assert(obj_value && obj_type);
		return e_res_eval_create(
			eval_rval_create(
				ast_type_copy(obj_type),
				value_copy(obj_value)
			)
		);
	}
	assert(value_issync(v));
	struct ast_type *t = ast_type_struct_membertype(
		eval_type(eval), field, state_getext(s)
	);
	return e_res_eval_create(
		eval_rval_create(
			ast_type_copy(t),
			value_rconst_create(
				ast_expr_member_create(
					value_as_rconst(v),
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
	struct e_res *r1 = ast_expr_pf_reduce(expr->u.binary.e1, s),
		     *r2 = ast_expr_pf_reduce(expr->u.binary.e2, s);

	/* TODO: user errors */
	struct value *v1 = eval_as_rval(e_res_as_eval(r1)),
		     *v2 = eval_as_rval(e_res_as_eval(r2));

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
range_geninstr(struct ast_expr *, struct lexememarker *, struct ast_block *,
		struct state *);

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
	case EXPR_RANGEBOUND:
		return expr;
	case EXPR_RANGE:
		return range_geninstr(expr, loc, b, s);
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
range_geninstr(struct ast_expr *expr, struct lexememarker *loc, struct ast_block *b,
		struct state *s)
{
	struct ast_expr *gen_lw = ast_expr_geninstr(
		ast_expr_range_lw(expr), loc, b, s
	);
	struct ast_expr *gen_up = ast_expr_geninstr(
		ast_expr_range_up(expr), loc, b, s
	);
	return ast_expr_range_create(gen_lw, gen_up);
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
