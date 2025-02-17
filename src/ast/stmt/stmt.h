#ifndef XR0_AST_STMT_STMT_H
#define XR0_AST_STMT_STMT_H

#include "util.h"

enum ast_stmt_kind {
	STMT_NOP,
	STMT_DECLARATION,
	STMT_LABELLED,
	STMT_COMPOUND,
	STMT_COMPOUND_V,
	STMT_EXPR,
	STMT_SELECTION,
	STMT_ITERATION,
	STMT_JUMP,
	STMT_ALLOCATION,
	STMT_REGISTER,
};

enum ast_stmt_kind
ast_stmt_kind(struct ast_stmt *);

bool
ast_stmt_issetup(struct ast_stmt *);

struct ast_block *
ast_stmt_iter_invariant(struct ast_stmt *);

#endif
