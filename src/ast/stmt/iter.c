#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "util.h"

#include "expr.h"
#include "stmt.h"

struct iter {
	enum iter_type {
		WHILE,
		DO,
		FOR,
	} type;

	struct ast_expr *cond; 

	struct ast_block *inv; /* may be NULL */
	struct ast_stmt *body;

	struct ast_stmt *for_init;
	struct ast_expr *for_update;
};

static struct iter *
iter_create(enum iter_type type, struct ast_expr *cond, struct ast_block *inv,
		struct ast_stmt *body)
{
	struct iter *iter = malloc(sizeof(struct iter));
	assert(iter);
	iter->type = type;
	iter->cond = cond;
	iter->inv = inv;
	iter->body = body;
	return iter;
}

struct iter *
iter_while_create(struct ast_expr *cond, struct ast_block *inv,
		struct ast_stmt *body)
{
	return iter_create(WHILE, cond, inv, body);
}

struct iter *
iter_for_create(struct ast_stmt *init, struct ast_stmt *cond,
		struct ast_expr *update, struct ast_block *inv,
		struct ast_stmt *body)
{
	struct iter *iter = iter_create(FOR, ast_stmt_as_expr(cond), inv, body);
	iter->for_init = init,
	iter->for_update = update;
	return iter;
}

struct iter *
iter_copy(struct iter *old)
{
	struct iter *new = iter_create(
		old->type,
		ast_expr_copy(old->cond),
		old->inv ? ast_block_copy(old->inv) : NULL,
		ast_stmt_copy(old->body)
	);
	if (old->type == FOR) {
		new->for_init = ast_stmt_copy(old->for_init);
		new->for_update = ast_expr_copy(old->for_update);
	}
	return new;
}

void
iter_destroy(struct iter *iter)
{
	ast_expr_destroy(iter->cond);
	if (iter->inv) {
		ast_block_destroy(iter->inv);
	}
	ast_stmt_destroy(iter->body);
	if (iter->type == FOR) {
		ast_stmt_destroy(iter->for_init);
		ast_expr_destroy(iter->for_update);
	}
	free(iter);
}

void
iter_sprint(struct iter *iter, int indent_level, struct strbuilder *b)
{
	assert(iter->type == FOR);

	char *init = ast_stmt_str(iter->for_init, indent_level),
	     *cond = ast_expr_str(iter->cond),
	     *body = ast_stmt_str(iter->body, indent_level),
	     *update = ast_expr_str(iter->for_update);

	char *inv = iter->inv ?
		ast_block_absstr(iter->inv, indent_level) : dynamic_str("");

	strbuilder_printf(
		b,
		"for (%s %s %s) ~ %s%s",
		init, cond, update, inv, body
	);

	free(init); free(cond); free(body); free(update); free(inv);
}

struct ast_block *
iter_inv(struct iter *iter)
{
	assert(iter->inv);
	return iter->inv;
}

static int
isone(struct ast_expr *);

int
iter_inwhile1form(struct iter *iter)
{
	switch (iter->type) {
	case WHILE:
		return isone(iter->cond);
	case FOR:
		return 0;
	default:
		assert(0);
	}
}

static int
isone(struct ast_expr *e)
{
	return ast_expr_isconstant(e) && ast_expr_as_constant(e) == 1;
}

struct string_arr *
iter_getfuncs(struct iter *iter)
{
	struct string_arr *arr = string_arr_concat(
		ast_expr_getfuncs(iter->cond), ast_stmt_getfuncs(iter->body)
	);
	/* XXX: inlucde loop abstracts potentially */
	if (iter->type == FOR) {
		arr = string_arr_concat(
			arr, 
			string_arr_concat(
				ast_stmt_getfuncs(iter->for_init),
				ast_expr_getfuncs(iter->for_update)
			)
		);
	}
	return arr;
}
