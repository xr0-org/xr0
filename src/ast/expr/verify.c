#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "expr.h"
#include "ext.h"
#include "intern.h"
#include "object.h"
#include "state.h"
#include "util.h"
#include "value.h"
#include "type.h"
#include "verifier.h"

struct bool_res *
ast_expr_decide(struct ast_expr *expr, struct state *s)
{
	struct ast_expr *prop = ast_expr_binary_create(
		ast_expr_copy(expr), BINARY_OP_NE, ast_expr_constant_create(0)
	);
	struct e_res *res = ast_expr_eval(prop, s);
	if (e_res_iserror(res)) {
		return bool_res_error_create(e_res_as_error(res));
	}
	return bool_res_bool_create(
		value_as_constant(eval_as_rval(e_res_as_eval(res)))
	);
}


/* ast_expr_eval */

static struct e_res *
directeval(struct ast_expr *, struct state *);

struct e_res *
ast_expr_eval(struct ast_expr *expr, struct state *state)
{
	struct e_res *res = directeval(expr, state);
	if (e_res_iserror(res) || !e_res_haseval(res)) {
		return res;
	}
	struct eval *eval = e_res_as_eval(res);
	struct ast_type *t = eval_type(eval);

	/* As per 3.2.2.1 and 3.3.2.1, if the type of the identifier is an array
	 * we convert to a pointer (to its first element, which follows
	 * implicitly from how variables are stored and returned by state_getloc
	 * above). */
	if (ast_type_isarr(t)) {
		struct value *v = value_ptr_create(
			location_copy(eval_as_lval(eval))
		);
		t = ast_type_create_ptr(ast_type_arr_type(ast_type_copy(t)));
		e_res_destroy(res);
		return e_res_eval_create(eval_rval_create(t, v));
	}

	return res;
}

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
expr_alloc_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_assign_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_incdec_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_binary_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
range_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
isdeallocand_eval(struct ast_expr *expr, struct state *state);

static struct e_res *
directeval(struct ast_expr *expr, struct state *state)
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
	case EXPR_ALLOCATION:
		return expr_alloc_eval(expr, state);
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
	case EXPR_ISDEALLOCAND:
		assert(state_modecanverify(state));
		return isdeallocand_eval(expr, state);
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

	struct loc_res *loc_res = state_getloc(state, id);
	if (loc_res_iserror(loc_res)) {
		return e_res_error_create(loc_res_as_error(loc_res));
	}

	struct location *loc = location_copy(loc_res_as_loc(loc_res));
	struct ast_type *t = ast_type_copy(state_getvariabletype(state, id));

	if (ast_type_isarr(t)) {
		return e_res_eval_create(
			eval_rval_create(
				ast_type_create_ptr(ast_type_arr_type(t)),
				value_ptr_create(loc)
			)
		);
	}

	return e_res_eval_create(eval_lval_create(t, loc));
}

static struct e_res *
hack_identifier_builtin_eval(char *id, struct state *state)
{
	if (state_hasrconst(state, id) || strncmp(id, "ptr:", 4) == 0) {
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
negative_eval(struct ast_expr *, struct state *);

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
	case UNARY_OP_NEGATIVE:
		return negative_eval(expr, state);
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
	a_printf(
		eval_islval(eval),
		/* TODO: analyse evaluation above to determine whether this is a
		 * user error or bug */
		"operand of address expression must evaluate to lvalue or "
		"function pointer\n"
	);
	struct value *v = value_ptr_create(location_copy(eval_as_lval(eval)));
	struct ast_type *t = ast_type_create_ptr(ast_type_copy(eval_type(eval)));
	e_res_destroy(res);
	return e_res_eval_create(eval_rval_create(ast_type_copy(t), v));
}

static struct e_res *
bang_eval(struct ast_expr *expr, struct state *state)
{
	struct bool_res *istrue_res = ast_expr_decide(
		ast_expr_unary_operand(expr), state
	);
	if (bool_res_iserror(istrue_res)) {
		return e_res_error_create(bool_res_as_error(istrue_res));
	}
	return e_res_eval_create(
		eval_rval_create(
			ast_type_create_int(),
			value_int_create(!bool_res_as_bool(istrue_res))
		)
	);
}

static struct e_res *
negative_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *operand = ast_expr_unary_operand(expr);
	struct e_res *res = ast_expr_eval(operand, state);
	if (e_res_iserror(res)) {
		return res;
	}
	struct eval *eval = e_res_as_eval(res);
	struct value_res *v_res = eval_to_value(eval, state);
	if (value_res_iserror(v_res)) {
		return e_res_error_create(value_res_as_error(v_res));
	}
	if (!value_res_hasvalue(v_res)) {
		char *s = ast_expr_str(expr);
		struct error *e = error_printf(
			"undefined memory access: %s has no value", s
		);
		free(s);
		return e_res_error_create(e);
	}
	return e_res_eval_create(
		eval_rval_create(
			eval_type(eval),
			value_int_create(
				-value_as_int(
					value_res_as_value(v_res), state
				)
			)
		)
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

static struct e_res *
pushcallframe(struct ast_expr *expr, struct state *state);

static struct e_res *
expr_call_eval(struct ast_expr *expr, struct state *state)
{
	return pushcallframe(expr, state);
}

static struct e_res *
pushcallframe(struct ast_expr *expr, struct state *state)
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

	struct frame *call_frame = frame_callabstract_create(
		ast_function_name(f),
		ast_function_abstract(f),
		ast_expr_copy(expr),
		f
	);
	state_pushframe(state, call_frame);

	if ((err = prepare_parameters(nparams, params, args, name, state))) {
		return e_res_error_create(err);
	}

	return e_res_empty_create();
}

static struct error *
setupverify(struct ast_expr *expr, struct state *state);

struct e_res *
ast_expr_setupverify(struct ast_expr *expr, struct state *state)
{
	/* XXX: no setup verification for alloc statement */
	if (ast_expr_kind(expr) != EXPR_ALLOCATION) {
		struct error *err = setupverify(expr, state);
		if (err) {
			return e_res_error_create(err);
		}
	}
	return e_res_empty_create();
}

static struct ast_block_res *
setupdecided_abstract(struct ast_expr *, struct state *);

static struct error *
setupverify(struct ast_expr *call, struct state *impl)
{
	char *fname = ast_expr_as_identifier(ast_expr_call_root(call));

	struct ast_function *f = externals_getfunc(state_getext(impl), fname);
	assert(f);

	struct state *spec = state_create(
		frame_callabstract_create(
			fname, ast_function_abstract(f), ast_expr_copy(call), f
		),
		rconst_create(),
		state_getext(impl)
	);
	int nparams = ast_function_nparams(f);
	struct ast_variable **param = ast_function_params(f);
	for (int i = 0; i < nparams; i++) {
		state_declare(spec, param[i], true);
	}

	struct ast_block_res *decided_abs_res = setupdecided_abstract(
		call, impl
	);
	if (ast_block_res_iserror(decided_abs_res)) {
		return ast_block_res_as_error(decided_abs_res);
	}
	state_pushframe(
		spec, 
		frame_blockfindsetup_create(
			dynamic_str("findsetup"),
			ast_block_res_as_block(decided_abs_res)
		)
	);

	while (!state_atsetupend(spec)) {
		struct error *err = state_step(spec);
		assert(!err);
	}
	assert(!state_atend(spec));

	state_popframe(spec);

	struct e_res *push_res = pushcallframe(call, impl);
	assert(!e_res_iserror(push_res));

	for (int i = 0; i < nparams; i++) {
		char *id = ast_variable_name(param[i]);
		struct error *err = state_constraintverify(spec, impl, id);
		if (err) {
			return error_printf(
				"precondition failure: argument of `%s' %w",
				id, err
			);
		}
	}

	state_popframe(impl);

	return NULL;
}

static struct ast_block_res *
setupdecided_abstract(struct ast_expr *call, struct state *s)
{
	struct ast_function *f = externals_getfunc(
		state_getext(s),
		ast_expr_as_identifier(ast_expr_call_root(call))
	);
	assert(f);

	/* modulation is by parameters in called function, so we prepare these
	 * first */
	struct e_res *push_res = pushcallframe(call, s);
	if (e_res_iserror(push_res)) {
		return ast_block_res_error_create(e_res_as_error(push_res));
	}

	struct ast_block_res *res = ast_block_setupdecide(
		ast_function_abstract(f), s
	);
	if (ast_block_res_iserror(res)) {
		assert(error_to_verifierinstruct(ast_block_res_as_error(res)));
	}

	/* pop call frame */
	state_popframe(s);

	return res;
}



static struct ast_type *
call_type(struct ast_expr *call, struct state *);

struct ast_type *
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
		struct object_res *o_res = state_get(
			state, eval_as_lval(e_res_as_eval(lval_res)), true
		);
		assert(!object_res_iserror(o_res));
		ast_expr_destroy(name);

		object_assign(object_res_as_object(o_res), value_copy(arg[i]));
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
additive_eval(struct ast_expr *, struct state *);

static struct e_res *
relational_eval(struct ast_expr *, struct state *);

static struct e_res *
expr_binary_eval(struct ast_expr *expr, struct state *state)
{
	switch (ast_expr_binary_op(expr)) {
	case BINARY_OP_ADDITION:
	case BINARY_OP_SUBTRACTION:
		return additive_eval(expr, state);
	case BINARY_OP_LT:
	case BINARY_OP_GT:
	case BINARY_OP_GE:
	case BINARY_OP_LE:
	case BINARY_OP_EQ:
	case BINARY_OP_NE:
		return relational_eval(expr, state);
	default:
		assert(false);
	}
}

static struct e_res *
value_additive_eval(struct eval *, enum ast_binary_operator, struct eval *,
		struct state *);

static struct e_res *
additive_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *e1 = ast_expr_binary_e1(expr),
			*e2 = ast_expr_binary_e2(expr);
	struct e_res *res1 = ast_expr_eval(e1, state),
		     *res2 = ast_expr_eval(e2, state);
	if (e_res_iserror(res1)) {
		return res1;
	}
	if (e_res_iserror(res2)) {
		return res2;
	}
	return value_additive_eval(
		e_res_as_eval(res1),
		ast_expr_binary_op(expr),
		e_res_as_eval(res2),
		state
	);
}

static struct e_res *
value_additive_eval(struct eval *rv1, enum ast_binary_operator op,
		struct eval *rv2, struct state *s)
{
	struct ast_type *t1 = eval_type(rv1),
			*t2 = eval_type(rv2);
	if (ast_type_isptr(t2)) {
		if (ast_type_isptr(t1)) {
			a_printf(false, "adding two pointers not supported\n");
		}
		return value_additive_eval(rv2, op, rv1, s);
	}
	/* ⊢ !ast_type_isptr(t2) */

	struct value_res *v_res1 = eval_to_value(rv1, s),
			 *v_res2 = eval_to_value(rv2, s);
	a_printf(
		value_res_hasvalue(v_res1) && value_res_hasvalue(v_res2),
		"undefined memory access needing better error message\n"
	);

	struct value *v1 = value_res_as_value(v_res1),
		     *v2 = value_res_as_value(v_res2);

	if (ast_type_isptr(t1)) {
		if (!value_islocation(v1)) {
			/* The Standard requires "a pointer to an object type",
			 * not merely a pointer-type. We are interpreting this
			 * to mean that the pointer must be valid (i.e. pointing
			 * at something) for the addition operation to take
			 * place. 3.3.6 */
			return e_res_error_create(
				error_printf("cannot add to invalid pointer")
			);
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
	/* ⊢ !ast_type_isptr(t1) */

	/* TODO: check t1, t2 are compatible arithmetic types */
	struct value *v = value_rconst_create(
		ast_expr_binary_create(
			value_to_expr(v1), op, value_to_expr(v2)
		)
	);
	return e_res_eval_create(eval_rval_create(t1, v));
}

static struct e_res *
value_relational_eval(struct eval *, enum ast_binary_operator op,
		struct eval *, struct state *);

static struct e_res *
relational_eval(struct ast_expr *expr, struct state *state)
{
	struct ast_expr *e1 = ast_expr_binary_e1(expr),
			*e2 = ast_expr_binary_e2(expr);
	struct e_res *res1 = ast_expr_eval(e1, state),
		     *res2 = ast_expr_eval(e2, state);
	if (e_res_iserror(res1)) {
		return res1;
	}
	if (e_res_iserror(res2)) {
		return res2;
	}
	return value_relational_eval(
		e_res_as_eval(res1),
		ast_expr_binary_op(expr),
		e_res_as_eval(res2),
		state
	);
}

static int
value_compare(struct value *, enum ast_binary_operator op,
		struct value *, struct state *);

static struct e_res *
value_relational_eval(struct eval *rv1, enum ast_binary_operator op,
		struct eval *rv2, struct state *s)
{
	a_printf(
		ast_type_isint(eval_type(rv1)) && ast_type_isint(eval_type(rv2)),
		"only comparisons between two integers are supported\n" 
	);
	struct value *v1 = value_res_as_value(eval_to_value(rv1, s)),
		     *v2 = value_res_as_value(eval_to_value(rv2, s));

	struct error *err;
	if ((err = value_disentangle(v1, v2, s))) {
		return e_res_error_create(err);
	}

	return e_res_eval_create(
		eval_rval_create(
			ast_type_create_int(),
			value_int_create(value_compare(v1, op, v2, s))
		)
	);
}


static int
value_compare(struct value *lhs, enum ast_binary_operator op,
		struct value *rhs, struct state *s)
{
	switch (op) {
	case BINARY_OP_EQ:
		return value_eq(lhs, rhs, s);
	case BINARY_OP_NE:
		return !value_eq(lhs, rhs, s);
	case BINARY_OP_LT:
		return value_lt(lhs, rhs, s);
	case BINARY_OP_GT:
		return value_lt(rhs, lhs, s);
	case BINARY_OP_GE:
		return value_eq(lhs, rhs, s) || value_lt(rhs, lhs, s);
	case BINARY_OP_LE:
		return value_eq(lhs, rhs, s) || value_lt(lhs, rhs, s);
	default:
		assert(false);
	}
}

static struct value *
range_rconst(struct ast_expr *, struct state *);

static struct e_res *
range_eval(struct ast_expr *expr, struct state *state)
{
	return e_res_eval_create(
		eval_rval_create(
			/* XXX: we will investigate type conversions later */
			ast_type_create_range(),
			range_rconst(expr, state)
		)
	);
}

static char *
modulatedkey(struct ast_expr *, struct state *);

static struct value *
range_rconst(struct ast_expr *expr, struct state *state)
{
	if (ast_expr_range_haskey(expr)) {
		return state_rconst(
			state,
			/* XXX: we will investigate type conversions later */
			ast_type_create_range(),
			expr,
			modulatedkey(expr, state),
			false
		);
	}
	return state_rconstnokey(
		state,
		/* XXX: we will investigate type conversions later */
		ast_type_create_range(),
		expr,
		false
	);
}

static char *
modulatedkey(struct ast_expr *e, struct state *s)
{
	struct strbuilder *b = strbuilder_create();
	char *mod = state_argmodulator(s);
	strbuilder_printf(b, "%s:{%s}", ast_expr_range_key(e), mod);
	free(mod);
	return strbuilder_build(b);
}

static struct object *
hack_object_from_assertion(struct ast_expr *, struct state *);

static struct e_res *
isdeallocand_eval(struct ast_expr *expr, struct state *state)
{
	struct object *obj = hack_object_from_assertion(expr, state);
	return e_res_eval_create(
		eval_rval_create(
			ast_type_create_int(),
			value_int_create(state_addresses_deallocand(state, obj))
		)
	);
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

static int
hack_constorone(struct ast_expr *, struct state *);

static struct e_res *
dealloc_process(struct ast_expr *, struct state *);

/* operates at location level. it either creates an object on the heap and
 * returns a location or gets the location pointed to by an lvalue and attempts
 * to free possibly returning an error */
static struct e_res *
expr_alloc_eval(struct ast_expr *expr, struct state *state)
{
	if (!state_modecanrunxr0cmd(state)) {
		return e_res_error_create(
			error_printf("Xr0 commands can only be run in abstract mode")
		);
	}

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
				state_clump(
					state,
					hack_constorone(
						ast_expr_alloc_arg(expr),
						state
					)
				)
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
		state_writeregister(
			state, value_copy(eval_as_rval(e_res_as_eval(res)))
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
	if (!object_hasvalue(obj)) {
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
		assert(expr);
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
	return ast_expr_range_create(
		dynamic_str(ast_expr_range_key(expr)), gen_lw, gen_up
	);
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
binary_geninstr(struct ast_expr *e, struct lexememarker *loc, struct ast_block *b,
		struct state *s)
{
	struct ast_expr *e1 = ast_expr_geninstr(ast_expr_binary_e1(e), loc, b, s),
			*e2 = ast_expr_geninstr(ast_expr_binary_e2(e), loc, b, s);
	return ast_expr_binary_create(e1, ast_expr_binary_op(e), e2);
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
	struct ast_expr	*gen_arg = ast_expr_geninstr(
		ast_expr_alloc_arg(expr), loc, b, s
	);
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
	return ast_expr_assignment_create(
		ast_expr_copy(lval), gen_rval
	);
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
	a_printf(f, "BAD ERROR: function `%s' not found\n", name);
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
