#ifndef XR0_AST_EXPR_H
#define XR0_AST_EXPR_H

#include "util.h"

enum ast_alloc_kind {
	ALLOC		= 1 << 0,
	DEALLOC		= 1 << 1,
	CLUMP		= 1 << 2,
};

struct ast_expr {
	enum ast_expr_kind {
		EXPR_IDENTIFIER		= 1 << 0,
		EXPR_CONSTANT		= 1 << 1,
		EXPR_STRING_LITERAL	= 1 << 2,
		EXPR_BRACKETED		= 1 << 3,
		EXPR_ITERATION		= 1 << 4,

		EXPR_CALL		= 1 << 5,
		EXPR_INCDEC		= 1 << 6,

		EXPR_STRUCTMEMBER	= 1 << 7,

		EXPR_UNARY		= 1 << 8,
		EXPR_BINARY		= 1 << 9,

		EXPR_ASSIGNMENT		= 1 << 10,

		EXPR_ISDEALLOCAND	= 1 << 11,
		EXPR_ISDEREFERENCABLE	= 1 << 12,
		EXPR_ARBARG		= 1 << 13,
		EXPR_ALLOCATION		= 1 << 14,
	} kind;
	struct ast_expr *root;
	union {
		char *string; /* identifier, literal, assertion */
		struct { 
			int constant;
			bool ischar;
		} constant;
		struct {
			int n;
			struct ast_expr **arg;
		} call;
		struct {
			int inc, pre;
		} incdec;
		enum ast_unary_operator {
			UNARY_OP_ADDRESS		= 1 << 0,
			UNARY_OP_DEREFERENCE		= 1 << 1,
			UNARY_OP_POSITIVE		= 1 << 2,
			UNARY_OP_NEGATIVE		= 1 << 3,
			UNARY_OP_ONES_COMPLEMENT	= 1 << 4,
			UNARY_OP_BANG			= 1 << 5,
		} unary_op;
		struct {
			enum ast_binary_operator {
				BINARY_OP_EQ	= 1 << 0,
				BINARY_OP_NE	= 1 << 1,

				BINARY_OP_LT	= 1 << 2,
				BINARY_OP_GT	= 1 << 3,
				BINARY_OP_LE	= 1 << 4,
				BINARY_OP_GE	= 1 << 5,

				BINARY_OP_ADDITION		= 1 << 6,
				BINARY_OP_SUBTRACTION		= 1 << 7,
				BINARY_OP_MULTIPLICATION	= 1 << 8,
			} op;
			struct ast_expr *e1, *e2;
		} binary;
		struct ast_expr *assignment_value;
		struct {
			enum ast_alloc_kind kind;
			struct ast_expr *arg;
		} alloc;
		char *arbarg_key;
	} u;
};

enum ast_expr_kind
ast_expr_kind(struct ast_expr *);

struct ast_expr *
ast_expr_binary_create(struct ast_expr *e1, enum ast_binary_operator,
		struct ast_expr *e2);

enum ast_binary_operator
ast_expr_binary_op(struct ast_expr *);

struct ast_stmt_splits
ast_expr_splits(struct ast_expr *, struct state *);

struct value_arr;
struct value_arr_res;

struct value_arr_res *
prepare_arguments(int nargs, struct ast_expr **arg, int nparams,
		struct ast_variable **param, struct state *state);

struct error *
prepare_parameters(int nparams, struct ast_variable **param, 
		struct value_arr *args, char *fname, struct state *state);

struct string_arr *
ast_expr_getfuncs(struct ast_expr *);

struct ast_type;
struct ast_declaration;

char *
ast_declaration_name(struct ast_declaration *);

struct ast_type *
ast_declaration_type(struct ast_declaration *);

struct ast_declaration *
ast_expr_declare(struct ast_expr *, struct ast_type *base);

struct ast_expr *
ast_expr_declarator(struct ast_expr *);

struct ast_expr *
ast_expr_initialiser(struct ast_expr *);

struct ast_type *
calloralloc_type(struct ast_expr *e, struct state *s);

#endif
