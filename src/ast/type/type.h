#ifndef XR0_AST_TYPE_H
#define XR0_AST_TYPE_H

enum ast_type_modifier {
	MOD_TYPEDEF	= 1 << 0,

	/* storage class */
	MOD_EXTERN	= 1 << 1,
	MOD_STATIC	= 1 << 2,
	MOD_AUTO	= 1 << 3,
	MOD_REGISTER	= 1 << 4,

	/* qualifier */
	MOD_CONST	= 1 << 5,
	MOD_VOLATILE	= 1 << 6,
};

struct ast_type *
ast_type_create(enum ast_type_base base, enum ast_type_modifier mod);

void
ast_type_mod_or(struct ast_type *, enum ast_type_modifier);

#endif
