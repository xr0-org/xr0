#include "stdio.h"
#include "assert.h"

#include "ast.h"
#include "state.h"
#include "value.h"

struct error *
ast_specval_verify(struct ast_type *t, struct value *param, struct value *arg,
		struct state *spec, struct state *caller)
{
	if (ast_type_isint(t)) {
		return value_confirmsubset(arg, param, caller, spec);
	} else if (ast_type_isstruct(t)) {
		return value_struct_specval_verify(param, arg, caller, spec);
	}
	a_printf(
		ast_type_isptr(t),
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
	if (!state_loc_valid(spec, param_ref)) {
		/* spec freed reference */
		return NULL;
	}
	if (!state_loc_valid(caller, arg_ref)) {
		return error_printf("must be lvalue");
	}

	if (state_loc_onheap(spec, param_ref)
			&& !state_loc_onheap(caller, arg_ref)) {
		return error_printf("must be heap allocated");
	}

	return state_specverify_block(spec, param_ref, caller, arg_ref, t);
}
