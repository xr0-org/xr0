#ifndef GRAM_UTIL_H
#define GRAM_UTIL_H

/* TODO: refactor this file out of existence */

struct expr_array {
	int n;
	struct ast_expr **expr;
};

struct stmt_array {
	int n;
	struct ast_stmt **stmt;
};

#endif
