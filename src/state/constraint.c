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
	char *varname;
	struct ast_type *t;	
	struct state *spec, *impl;
};

struct constraint *
constraint_create(char *varname, struct ast_type *t, struct state *spec,
		struct state *impl)
{
	struct constraint *c = malloc(sizeof(struct constraint));
	assert(c);
	c->varname = varname;
	c->t = t;
	c->spec = spec;
	c->impl = impl;
	return c;
}

void
constraint_destroy(struct constraint *c)
{
	free(c->varname);
	ast_type_destroy(c->t);
	free(c);
}

static int
_isrconst(struct ast_type *, struct value *);

struct error *
constraint_shapeverify(struct constraint *c, struct object *spec_obj,
		struct object *impl_obj)
{
	if (!object_isdef(spec_obj)) {
		return NULL;
	}

	struct value *spec_v = object_as_defvalue(spec_obj),
		     *impl_v = object_as_defvalue(impl_obj);

	if (value_isundef(spec_v)) {
		return NULL;
	} else if (ast_type_isarr(c->t)) {
		/* array-pointer conversion */
		struct constraint *new_c = constraint_create(
			dynamic_str(c->varname),
			ast_type_create_ptr(
				ast_type_arr_type(ast_type_copy(c->t))
			),
			c->spec,
			c->impl
		);
		struct error *err = constraint_shapeverify(
			new_c, spec_obj, impl_obj
		);
		constraint_destroy(new_c);
		return err;
	} else if (_isrconst(c->t, spec_v)) {
		return NULL;
	} else if (ast_type_isstruct(c->t)) {
		return value_struct_shapeverify(
			spec_v, impl_v, c->spec, c->impl
		);
	}
	a_printf(
		ast_type_isptr(c->t),
		"can only verify int, pointer and struct params: "\
		"have %s with values %s and %s\n",
		ast_type_str(c->t), value_str(spec_v), value_str(impl_v)
	);

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

	/* now we validate that the block referred to by impl_loc satisfies that
	 * referred to by spec_loc. the analysis is simplified by imposing the
	 * assumption that any permissivity as to the size and contents of impl
	 * block is encoded into the spec block. this means that, relative to
	 * the offsets indicated by impl_loc and spec_loc, the impl block must
	 * fit entirely into the spec block, and its contents within the region
	 * indicated by the spec block's dimensions must satisfy the constraints
	 * of the objects in spec block. it should be possible, therefore, to
	 * construct a 1-1 mapping from the spec block onto the impl block,
	 * mapping spec object onto satisfying impl object.
	 *
	 * in the constant case the requirement that whatever is permitted in
	 * the impl block must be encoded in the spec block implies the blocks
	 * must be of the same size and the offsets of the two locations be
	 * identical.
	 */

	struct ast_expr *spec_o = offset_as_expr(location_offset(spec_loc)),
			*impl_o = offset_as_expr(location_offset(impl_loc));
	a_printf(
		ast_expr_isconstant(spec_o) && ast_expr_isconstant(impl_o),
		"only constant offsets supported: have %s and %s\n", 
		ast_expr_str(spec_o), ast_expr_str(impl_o)
	);
	a_printf(
		ast_expr_as_constant(spec_o) == ast_expr_as_constant(impl_o),
		"unequal offsets: have %s and %s\n", 
		ast_expr_str(spec_o), ast_expr_str(impl_o)
	);

	struct block *spec_b = state_getblock(c->spec, spec_loc),
		     *impl_b = state_getblock(c->impl, impl_loc);
	assert(spec_b && impl_b);
	return block_shapeverify(
		spec_b, impl_b, c->spec, c->impl, c->varname, ast_type_deref(c->t)
	);
}

static int
_isrconst(struct ast_type *t, struct value *v)
{
	return ast_type_isint(t) || (ast_type_isptr(t) && !value_islocation(v));
}
