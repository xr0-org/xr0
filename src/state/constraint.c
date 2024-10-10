#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "state.h"
#include "value.h"

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

	return state_specverify_block(c->spec, param_ref, c->impl, arg_ref, c->t);
}
