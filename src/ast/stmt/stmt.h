#ifndef XR0_AST_STMT_H
#define XR0_AST_STMT_H

#include "util.h"

enum ast_stmt_kind {
	STMT_NOP		= 1 << 0,
	STMT_DECLARATION	= 1 << 1,
	STMT_LABELLED		= 2 << 2,
	STMT_COMPOUND		= 2 << 3,
	STMT_COMPOUND_V		= 2 << 4,
	STMT_EXPR		= 2 << 5,
	STMT_SELECTION		= 2 << 6,
	STMT_ITERATION		= 2 << 7,
	STMT_ITERATION_E	= 2 << 8,
	STMT_JUMP		= 2 << 9,
	STMT_ALLOCATION		= 2 << 10,
	STMT_REGISTER		= 2 << 11,
};

enum ast_stmt_kind
ast_stmt_kind(struct ast_stmt *);

bool
ast_stmt_ispre(struct ast_stmt *stmt);

bool
ast_stmt_isassume(struct ast_stmt *stmt);

struct string_arr *
ast_stmt_getfuncs(struct ast_stmt *stmt);

struct error;

struct state;

struct error *
ast_stmt_precondsinit(struct ast_stmt *, struct state *);

#endif
