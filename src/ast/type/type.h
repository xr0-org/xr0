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

enum ast_type_base { /* base type */
	/* variable type */
	TYPE_VOID,
	TYPE_CHAR,
	TYPE_SHORT,
	TYPE_INT,
	TYPE_LONG,
	TYPE_FLOAT,
	TYPE_DOUBLE,
	TYPE_SIGNED,
	TYPE_UNSIGNED,

	/* derived */
	TYPE_POINTER,
	TYPE_ARRAY,
	TYPE_STRUCT,
	TYPE_UNION,
	TYPE_USERDEF,

	TYPE_ENUM,

	TYPE_RANGE,
};

struct ast_type *
ast_type_create(enum ast_type_base base, enum ast_type_modifier mod);

void
ast_type_mod_or(struct ast_type *, enum ast_type_modifier);

struct externals;
struct namedseq;
struct lexememarker;

struct ast_expr *
ast_type_rconstgeninstr(struct ast_type *, struct namedseq *,
		struct lexememarker *, struct ast_block *, struct externals *);


#endif
