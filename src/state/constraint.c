#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "object.h"
#include "state.h"
#include "value.h"

#include "block.h"

struct constraint {
	struct state *spec, *impl;
	struct ast_type *t;	
};

struct constraint *
constraint_create(struct state *spec, struct state *impl, struct ast_type *t)
{
	struct constraint *c = malloc(sizeof(struct constraint));
	assert(c);
	c->spec = spec;
	c->impl = impl;
	c->t = t;
	return c;
}

void
constraint_destroy(struct constraint *c)
{
	ast_type_destroy(c->t);
	state_destroy(c->spec);
	state_destroy(c->impl);
	free(c);
}

static struct error *
verifyloc(struct constraint *c, struct location *spec, struct location *impl);

struct error *
constraint_verify(struct constraint *c, struct value *param, struct value *arg)
{
	if (ast_type_isint(c->t)) {
		return value_confirmsubset(arg, param, c->impl, c->spec);
	} else if (ast_type_isstruct(c->t)) {
		return value_struct_specval_verify(param, arg, c->impl, c->spec);
	}
	a_printf(
		ast_type_isptr(c->t),
		"can only verify int, struct and pointer params\n"
	);

	if (!value_islocation(param)) {
		/* allow for NULL and other invalid-pointer setups */
		return NULL;
	}
	/* spec requires value be valid pointer */
	if (!value_islocation(arg)) {
		return error_printf("must be pointing at something");
	}

	struct location *param_ref = value_as_location(param),
			*arg_ref = value_as_location(arg);
	if (!state_loc_valid(c->spec, param_ref)) {
		/* spec freed reference */
		return NULL;
	}
	if (!state_loc_valid(c->impl, arg_ref)) {
		return error_printf("must be lvalue");
	}

	if (state_loc_onheap(c->spec, param_ref)
			&& !state_loc_onheap(c->impl, arg_ref)) {
		return error_printf("must be heap allocated");
	}

	return verifyloc(c, param_ref, arg_ref);
}

static struct error *
verifyloc(struct constraint *c, struct location *spec, struct location *impl)
{
	struct block *b_param = state_getblock(c->spec, spec);
	assert(b_param);
	return block_constraintverify(b_param, impl, c);
}

struct error *
constraint_verifyobject(struct constraint *c, struct object *spec_obj,
		struct location *impl_loc)
{
	struct block *b_impl = state_getblock(c->impl, impl_loc);
	assert(b_impl);
	struct ast_expr *offset = ast_expr_sum_create(
		offset_as_expr(location_offset(impl_loc)),
		object_lower(spec_obj) /* XXX: assuming lower is offset */
	);
	struct object_res *res = block_observe(b_impl, offset, c->impl, false);
	if (object_res_iserror(res)) {
		struct error *err = object_res_as_error(res);
		if (error_to_block_observe_noobj(err)) {
			return error_printf("must have object");
		}
		printf("err: %s\n", error_str(err));
		assert(false);
	}
	struct object *arg_obj = object_res_as_object(res);
	if (!object_hasvalue(spec_obj)) {
		return NULL;
	}
	if (!object_hasvalue(arg_obj)) {
		return error_printf("must have value");
	}
	return constraint_verify(
		c, object_as_value(spec_obj), object_as_value(arg_obj)
	);
}
