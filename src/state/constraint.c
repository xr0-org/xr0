#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "object.h"
#include "state.h"
#include "value.h"
#include "lsi.h"

#include "block.h"
#include "constraint.h"

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
	free(c);
}

static int
size_le(struct location *spec_loc, struct location *impl_loc, struct state *spec,
		struct state *impl);

/* location_reloffset: return a location pointing at the same block as l1 but
 * with an offset that is the difference between l1's and l2's offset. */
static struct location *
location_reloffset(struct location *l1, struct location *l2);

struct lv_res *
constraint_verify(struct constraint *c, struct value *spec_v,
		struct value *impl_v)
{
	if (ast_type_isint(c->t)) {
		struct lsi_varmap *lv = lsi_varmap_create();
		lsi_varmap_set(
			lv,
			value_to_rconstid(spec_v, c->spec),
			value_to_rconstid(impl_v, c->impl)
		);
		return lv_res_lv_create(lv);
	} else if (ast_type_isstruct(c->t)) {
		return value_struct_specval_verify(
			spec_v, impl_v, c->impl, c->spec
		);
	}
	a_printf(
		ast_type_isptr(c->t),
		"can only verify int, struct and pointer params\n"
	);

	if (!value_islocation(spec_v)) {
		/* allow for NULL and other invalid-pointer setups */
		/* TODO: include non-location pointers above case */
		assert(0);
		return lv_res_lv_create(lsi_varmap_create());
	}
	/* spec requires value be valid pointer */
	if (!value_islocation(impl_v)) {
		return lv_res_error_create(
			error_printf("must be pointing at something")
		);
	}

	struct location *spec_loc = value_as_location(spec_v),
			*impl_loc = value_as_location(impl_v);
	if (!state_loc_valid(c->spec, spec_loc)) {
		/* spec freed reference */
		return lv_res_lv_create(lsi_varmap_create());
	}
	if (!state_loc_valid(c->impl, impl_loc)) {
		return lv_res_error_create(error_printf("must be lvalue"));
	}

	if (state_loc_onheap(c->spec, spec_loc)
			&& !state_loc_onheap(c->impl, impl_loc)) {
		return lv_res_error_create(
			error_printf("must be heap allocated")
		);
	}

	if (!size_le(spec_loc, impl_loc, c->spec, c->impl)) {
		return lv_res_error_create(
			error_printf("must point at larger block")
		);
	}

	/* we shift the impl_loc's offset by spec_loc's so that
	 * block_constraintverify can behave as though both were offset zero */
	struct location *rel_impl_loc = location_reloffset(impl_loc, spec_loc);
	struct block *spec_b = state_getblock(c->spec, spec_loc);
	assert(spec_b);
	struct lv_res *res = block_constraintverify(spec_b, rel_impl_loc, c);
	location_destroy(rel_impl_loc);
	return res;
}

static struct location *
location_reloffset(struct location *l1, struct location *l2)
{
	struct ast_expr *offset = ast_expr_difference_create(
		ast_expr_copy(offset_as_expr(location_offset(l1))),
		ast_expr_copy(offset_as_expr(location_offset(l2)))
	);
	struct location *offset_loc = location_copy(l1);
	location_setoffset(
		offset_loc, offset_create(ast_expr_copy(offset))
	);
	return offset_loc;
}

static int
size_le(struct location *spec_loc, struct location *impl_loc, struct state *spec,
		struct state *impl)
{
	struct block *spec_b = state_getblock(spec, spec_loc),
		     *impl_b = state_getblock(impl, impl_loc);
	assert(spec_b && impl_b);
	return block_size_le(spec_b, impl_b);
}

struct lv_res *
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
		return lv_res_error_create(
			error_to_block_observe_noobj(err)
			? error_printf("must have object")
			: err
		);
	}
	struct object *arg_obj = object_res_as_object(res);
	if (!object_hasvalue(spec_obj)) {
		return lv_res_lv_create(lsi_varmap_create());
	}
	if (!object_hasvalue(arg_obj)) {
		return lv_res_error_create(error_printf("must have value"));
	}
	return constraint_verify(
		c, object_as_value(spec_obj), object_as_value(arg_obj)
	);
}
