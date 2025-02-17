#ifndef XR0_AST_STMT_ITER_H
#define XR0_AST_STMT_ITER_H

struct iter;

struct ast_block;
struct ast_expr;
struct ast_stmt;

struct iter *
iter_for_create(struct ast_stmt *init, struct ast_stmt *cond,
		struct ast_expr *update, struct ast_block *inv,
		struct ast_stmt *body);

struct iter *
iter_copy(struct iter *);

void
iter_destroy(struct iter *);

struct strbuilder;

void
iter_sprint(struct iter *iter, int indent, struct strbuilder *);

struct ast_block *
iter_inv(struct iter *);

int
iter_inwhile1form(struct iter *);

struct lexememarker;

struct ast_block *
iter_while1form(struct iter *, struct lexememarker *);

struct string_arr;

struct string_arr *
iter_getfuncs(struct iter *);

#endif
