#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
#include "state.h"
#include "util.h"

#include "expr.h"
#include "iter.h"
#include "stmt.h"

struct iter {
	enum type {
		FOR,
	} type;

	struct ast_expr *cond; 

	struct ast_block *inv; /* may be NULL */
	struct ast_stmt *body;

	struct ast_stmt *for_init;
	struct ast_expr *for_update;
};

static struct iter *
iter_create(enum type type, struct ast_expr *cond, struct ast_block *inv,
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

static void
for_sprint(struct iter *, int indent, struct strbuilder *);

void
iter_sprint(struct iter *iter, int indent, struct strbuilder *b)
{
	switch (iter->type) {
	case FOR:
		for_sprint(iter, indent, b);
		break;
	default:
		assert(false);
	}
}

static void
for_sprint(struct iter *iter, int indent, struct strbuilder *b)
{
	char *init = ast_stmt_str(iter->for_init, indent),
	     *cond = ast_expr_str(iter->cond),
	     *body = ast_stmt_str(iter->body, indent),
	     *update = ast_expr_str(iter->for_update);

	char *inv = iter->inv ?
		ast_block_absstr(iter->inv, indent) : dynamic_str("");

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

static struct ast_block *
for_gotoform(struct iter *, struct lexememarker *);

struct ast_block *
iter_gotoform(struct iter *iter, struct lexememarker *loc)
{
	switch (iter->type) {
	case FOR:
		return for_gotoform(iter, loc);
	default:
		assert(false);
	}
}

static struct ast_block *
for_gotoform(struct iter *old, struct lexememarker *loc)
{
	printf("oldinv:\n%s\n", ast_block_str(old->inv, 1));
	struct ast_block *b = ast_block_create(NULL, 0);
	ast_block_append_stmt(b, ast_stmt_copy(old->for_init));
	ast_block_append_stmt(
		b,
		ast_stmt_create_labelled(
			lexememarker_copy(loc),
			dynamic_str(".start"),
			ast_stmt_create_compound_v(
				lexememarker_copy(loc),
				ast_block_copy(old->inv)
			)
		)
	);
	ast_block_append_stmt(
		b,
		ast_stmt_create_sel(
			lexememarker_copy(loc),
			false,
			ast_expr_unary_create(
				ast_expr_copy(old->cond), UNARY_OP_BANG
			),
			ast_stmt_create_goto(
				lexememarker_copy(loc),
				dynamic_str(".end")
			),
			NULL
		)
	);
	ast_block_append_stmt(b, ast_stmt_copy(old->body));
	ast_block_append_stmt(
		b,
		ast_stmt_create_expr(
			lexememarker_copy(loc),
			ast_expr_copy(old->for_update)
		)
	);
	ast_block_append_stmt(
		b,
		ast_stmt_create_goto(
			lexememarker_copy(loc),
			dynamic_str(".start")
		)
	);
	ast_block_append_stmt(
		b,
		ast_stmt_create_labelled(
			lexememarker_copy(loc),
			dynamic_str(".end"),
			ast_stmt_create_nop(lexememarker_copy(loc))
		)
	);
	return b;
}

static struct ast_block *
toblock(struct ast_stmt *);

void
iter_pushstatebody(struct iter *iter, struct state *s)
{
	/* TODO: clean up block somehow */
	state_pushloopframe(s, toblock(iter->body));
}

static struct ast_block *
toblock(struct ast_stmt *stmt)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_COMPOUND:
		return ast_block_copy(ast_stmt_as_block(stmt));
	case STMT_NOP:
	case STMT_SELECTION:
	case STMT_EXPR:
		break;
	default:
		assert(false);
	}
	struct ast_block *b = ast_block_create(NULL, 0);
	ast_block_append_stmt(b, ast_stmt_copy(stmt));
	return b;
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
