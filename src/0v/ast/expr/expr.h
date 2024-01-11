#ifndef XR0_AST_EXPR_H
#define XR0_AST_EXPR_H

#include "util.h"

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
		EXPR_ARBARG		= 1 << 12,
	} kind;
	struct ast_expr *root;
	union {
		char *string; /* identifier, literal, assertion */
		int constant;
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

				BINARY_OP_ADDITION	= 1 << 6,
				BINARY_OP_SUBTRACTION	= 1 << 7,
			} op;
			struct ast_expr *e1, *e2;
		} binary;
		struct ast_expr *assignment_value;
	} u;
};

enum ast_expr_kind
ast_expr_kind(struct ast_expr *);

struct ast_expr *
ast_expr_binary_create(struct ast_expr *e1, enum ast_binary_operator,
		struct ast_expr *e2);

enum ast_binary_operator
ast_expr_binary_op(struct ast_expr *expr);

struct string_arr *
ast_expr_getfuncs(struct ast_expr *);

#endif
