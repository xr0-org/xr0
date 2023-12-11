#ifndef XR0_AST_STMT_H
#define XR0_AST_STMT_H

enum ast_stmt_kind {
	STMT_NOP		= 1 << 0,
	STMT_LABELLED		= 1 << 1,
	STMT_COMPOUND		= 1 << 2,
	STMT_COMPOUND_V		= 1 << 3,
	STMT_EXPR		= 1 << 4,
	STMT_SELECTION		= 1 << 5,
	STMT_ITERATION		= 1 << 6,
	STMT_ITERATION_E	= 1 << 7,
	STMT_JUMP		= 1 << 8,
	STMT_ALLOCATION		= 1 << 9,
};

enum ast_stmt_kind
ast_stmt_kind(struct ast_stmt *);

#endif
