#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "state.h"
#include "util.h"
#include "value.h"

static struct result *
iter_absexec(struct ast_stmt *stmt, struct state *state);

static struct result *
comp_absexec(struct ast_stmt *stmt, struct state *state);

struct result *
ast_stmt_absexec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_LABELLED:
		/* ignore labelled statements for now */
		return result_value_create(NULL);
	case STMT_EXPR:
		return ast_expr_absexec(ast_stmt_as_expr(stmt), state);
	case STMT_ITERATION:
		return iter_absexec(stmt, state);
	case STMT_COMPOUND:
		return comp_absexec(stmt, state);
	default:
		assert(false);
	}
}

static struct ast_expr *
hack_mem_from_neteffect(struct ast_stmt *);

static struct object *
hack_base_object_from_mem(struct ast_expr *, struct state *);

static struct result *
iter_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct error *err;

	struct ast_expr *mem = hack_mem_from_neteffect(stmt);

	struct object *obj = hack_base_object_from_mem(mem, state);

	struct ast_expr *lw = ast_stmt_iter_lower_bound(stmt),
			*up = ast_stmt_iter_upper_bound(stmt);

	struct result *result_lw = ast_expr_eval(lw, state),
		      *result_up = ast_expr_eval(up, state);

	if (result_iserror(result_lw)) {
		return result_lw;
	}
	if (result_iserror(result_up)) {
		return result_up;
	}

	struct ast_expr *res_lw = value_to_expr(result_as_value(result_lw)),
			*res_up = value_to_expr(result_as_value(result_up));

	result_destroy(result_up);
	result_destroy(result_lw);

	if (ast_expr_memory_isalloc(mem)) {
		err = state_range_alloc(state, obj, res_lw, res_up);
	} else {
		err = state_range_dealloc(state, obj, res_lw, res_up);
	}
	
	ast_expr_destroy(res_up);
	ast_expr_destroy(res_lw);

	if (err) {
		return result_error_create(err);
	}

	return result_value_create(NULL);
}

static struct ast_expr *
hack_mem_from_neteffect(struct ast_stmt *stmt)
{
	struct ast_stmt *body = ast_stmt_iter_body(stmt);
	assert(ast_stmt_kind(body) == STMT_COMPOUND);
	struct ast_block *block = ast_stmt_as_block(body);
	assert(ast_block_ndecls(block) == 0 && ast_block_nstmts(block) == 1);
	return ast_stmt_as_expr(ast_block_stmts(block)[0]);
}

static struct object *
hack_base_object_from_mem(struct ast_expr *expr, struct state *state)
{
	/* we're currently discarding analysis of `offset` and relying on the
	 * bounds (lower, upper beneath) alone */
	struct ast_expr *acc = ast_expr_memory_root(expr); /* `*(arr+offset)` */
	assert(ast_expr_unary_op(acc) == UNARY_OP_DEREFERENCE);
	struct ast_expr *inner = ast_expr_unary_operand(acc); /* `arr+offset` */
	struct ast_expr *i = ast_expr_identifier_create(dynamic_str("i"));
	assert(ast_expr_equal(ast_expr_binary_e2(inner), i)); 
	ast_expr_destroy(i);
	struct object *obj = lvalue_object(
		ast_expr_lvalue(ast_expr_binary_e1(inner), state)
	);
	assert(obj);
	return obj;
}


static struct result *
comp_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct ast_block *b = ast_stmt_as_block(stmt);
	struct ast_stmt **stmts = ast_block_stmts(b);
	for (int i = 0; i < ast_block_nstmts(b); i++) {
		struct result *res = ast_stmt_absexec(stmts[i], state);
		if (result_iserror(res)) {
			return res;
		}
	}
	return result_value_create(NULL);
}
