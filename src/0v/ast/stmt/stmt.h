#ifndef XR0_AST_STMT_H
#define XR0_AST_STMT_H

#include "util.h"

enum ast_alloc_kind {
	ALLOC			= 1 << 0,
	DEALLOC			= 1 << 1,
	CLUMP			= 1 << 2,
};

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

bool
ast_stmt_ispre(struct ast_stmt *stmt);

bool
ast_stmt_isassume(struct ast_stmt *stmt);

struct string_arr *
ast_stmt_getfuncs(struct ast_stmt *stmt);

struct ast_stmt_splits {
	int n;
	struct ast_expr **cond;
};

struct ast_stmt_splits
ast_stmt_splits(struct ast_stmt *, struct state *);

struct state;
struct error;

/* TODO: change to more regular tuple */
struct decision { bool decision; struct error *err; }
sel_decide(struct ast_expr *control, struct state *state);

struct error *
ast_stmt_precondsinit(struct ast_stmt *, struct state *);

struct error *
ast_stmt_precondsverify(struct ast_stmt *, struct state *);

#endif
