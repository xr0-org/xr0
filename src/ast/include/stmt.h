#ifndef XR0_AST_STMT_H
#define XR0_AST_STMT_H

struct string_arr;

struct string_arr *
ast_stmt_getfuncs(struct ast_stmt *stmt);

struct ast_stmt *
ast_stmt_create_for(struct lexememarker *loc, struct ast_stmt *init,
		struct ast_stmt *cond, struct ast_expr *update,
		struct ast_block *inv, struct ast_stmt *body);

int
ast_stmt_isjump(struct ast_stmt *);

int
ast_stmt_isreturn(struct ast_stmt *);

int
ast_stmt_isbreak(struct ast_stmt *);

#endif
