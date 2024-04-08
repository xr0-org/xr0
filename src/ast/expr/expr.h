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
		enum ast_unary_operator unary_op;
		struct {
			enum ast_binary_operator op;
			struct ast_expr *e1, *e2;
		} binary;
		struct ast_expr *assignment_value;
		struct {
			enum ast_alloc_kind kind;
			struct ast_expr *arg;
		} alloc;
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

struct result_arr {
	int n;
	struct result **res;
};

struct result_arr *
result_arr_create();

void
result_arr_destroy(struct result_arr *arr);

void
result_arr_append(struct result_arr *arr, struct result *res);

struct result_arr *
prepare_arguments(int nargs, struct ast_expr **arg, int nparams,
		struct ast_variable **param, struct state *state);

struct error *
prepare_parameters(int nparams, struct ast_variable **param, 
		struct result_arr *args, char *fname, struct state *state);

struct string_arr *
ast_expr_getfuncs(struct ast_expr *);

#endif
