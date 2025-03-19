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

struct error *
constraint_shapeverify(struct constraint *c, struct value *spec_v,
		struct value *impl_v)
{
	if (ast_type_isint(c->t)) {
		return NULL;
	} else if (ast_type_isstruct(c->t)) {
		return value_struct_shapeverify(
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
		return NULL;
	}
	/* spec requires value be valid pointer */
	if (!value_islocation(impl_v)) {
		return error_printf("must be pointing at something");
	}

	struct location *spec_loc = value_as_location(spec_v),
			*impl_loc = value_as_location(impl_v);
	if (!state_loc_valid(c->spec, spec_loc)) {
		/* spec freed reference */
		return NULL;
	}
	if (!state_loc_valid(c->impl, impl_loc)) {
		return error_printf("must be lvalue");
	}

	if (state_loc_onheap(c->spec, spec_loc)
			&& !state_loc_onheap(c->impl, impl_loc)) {
		return error_printf("must be heap allocated");
	}

	if (!size_le(spec_loc, impl_loc, c->spec, c->impl)) {
		return error_printf("must point at larger block");
	}

	/* we shift the impl_loc's offset by spec_loc's so that
	 * block_constraintverify can behave as though both were offset zero */
	struct location *rel_impl_loc = location_reloffset(impl_loc, spec_loc);
	struct block *spec_b = state_getblock(c->spec, spec_loc);
	assert(spec_b);
	struct error *err = block_shapeverify(spec_b, rel_impl_loc, c);
	location_destroy(rel_impl_loc);
	return err;
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

struct lsi_varmap *
constraint_impl_spec_mapping(struct constraint *c, struct value *spec_v,
		struct value *impl_v, char *alias)
{
	if (ast_type_isint(c->t)) {
		struct lsi_varmap *lv = lsi_varmap_create();
		char *spec_rconst = value_to_rconstid(spec_v, c->spec);
		lsi_varmap_set(
			lv,
			value_to_rconstid(impl_v, c->impl),
			dynamic_str(spec_rconst)
		);
		lsi_varmap_setvaluealias(
			lv,
			dynamic_str(spec_rconst),
			dynamic_str(alias)
		);
		free(spec_rconst);
		return lv;
	} else if (ast_type_isstruct(c->t)) {
		return value_struct_impl_spec_mapping(
			spec_v, impl_v, c->impl, c->spec, alias
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
		return lsi_varmap_create();
	}

	struct location *spec_loc = value_as_location(spec_v),
			*impl_loc = value_as_location(impl_v);
	if (!state_loc_valid(c->spec, spec_loc)) {
		/* spec freed reference */
		return lsi_varmap_create();
	}
	assert(state_loc_valid(c->impl, impl_loc));

	/* we shift the impl_loc's offset by spec_loc's so that
	 * block_constraintverify can behave as though both were offset zero */
	struct location *rel_impl_loc = location_reloffset(impl_loc, spec_loc);
	struct block *spec_b = state_getblock(c->spec, spec_loc);
	assert(spec_b);
	struct lsi_varmap *lv = block_impl_spec_mapping(
		spec_b, rel_impl_loc, c, alias
	);
	location_destroy(rel_impl_loc);
	return lv;
}

struct error *
constraint_shapeverify_object(struct constraint *c, struct object *spec_obj,
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
		return error_to_block_observe_noobj(err)
			? error_printf("must have object")
			: err;
	}
	struct object *arg_obj = object_res_as_object(res);
	if (!object_hasvalue(spec_obj)) {
		return NULL;
	}
	if (!object_hasvalue(arg_obj)) {
		return error_printf("must have value");
	}
	return constraint_shapeverify(
		c, object_as_value(spec_obj), object_as_value(arg_obj)
	);
}

struct lsi_varmap *
constraint_impl_spec_mapping_object(struct constraint *c,
		struct object *spec_obj, struct location *impl_loc, char *alias)
{
	struct block *b_impl = state_getblock(c->impl, impl_loc);
	assert(b_impl);
	struct ast_expr *offset = ast_expr_sum_create(
		offset_as_expr(location_offset(impl_loc)),
		object_lower(spec_obj) /* XXX: assuming lower is offset */
	);
	struct object_res *res = block_observe(b_impl, offset, c->impl, false);
	struct object *arg_obj = object_res_as_object(res);
	if (!object_hasvalue(spec_obj)) {
		return lsi_varmap_create();
	}
	return constraint_impl_spec_mapping(
		c, object_as_value(spec_obj), object_as_value(arg_obj), alias
	);
}
