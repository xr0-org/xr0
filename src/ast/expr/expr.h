#ifndef XR0_AST_EXPR_H
#define XR0_AST_EXPR_H

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
		enum ast_unary_operator unary_op;
		struct {
			enum ast_binary_operator op;
			struct ast_expr *e1, *e2;
		} binary;
		struct ast_expr *assignment_value;
	} u;
};

enum ast_expr_kind
ast_expr_kind(struct ast_expr *);

#endif
