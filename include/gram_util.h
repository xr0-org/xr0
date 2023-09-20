#ifndef GRAM_UTIL_H
#define GRAM_UTIL_H

struct expr_array {
	int n;
	struct ast_expr **expr;
};

struct variable_array {
	int n;
	struct ast_variable **var;
};

struct stmt_array {
	int n;
	struct ast_stmt **stmt;
};

#endif
