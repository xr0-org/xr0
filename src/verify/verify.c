#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ext.h"
#include "ast.h"
#include "object.h"
#include "state.h"
#include "util.h"
#include "value.h"
#include "verify.h"

static struct error *
stmt_verify(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state,
		struct externals *);

static struct error *
parameterise_state(struct state *s, struct ast_function *f);

struct error *
path_verify(struct ast_function *f, struct state *state, struct externals *ext)
{
	struct error *err = NULL;

	struct ast_block *body = ast_function_body(f);

	if ((err = parameterise_state(state, f))) {
		return err;
	}

	int ndecls = ast_block_ndecls(body);
	struct ast_variable **var = ast_block_decls(body);
	for (int i = 0; i < ndecls; i++) {
		state_declare(state, var[i], false);
	}

	int nstmts = ast_block_nstmts(body);
	struct ast_stmt **stmt = ast_block_stmts(body);
	for (int i = 0; i < nstmts; i++) {
		struct ast_stmt *s = stmt[i];
		/* TODO: deal with pathing logic so that we only have one path
		 * to terminal points for the function */
		enum ast_stmt_kind kind = ast_stmt_kind(s);
		if (kind == STMT_COMPOUND_V) {
			if ((err = stmt_verify(s, state))) {
				return error_prepend(err, "cannot verify statement: ");
			}
		}
		if ((err = stmt_exec(s, state))) {
			return error_prepend(err, "cannot exec statement: ");
		}
	}
	state_undeclarevars(state);
	/* TODO: verify that `result' is of same type as f->result */
	if ((err = abstract_audit(f, state, ext))) {
		return error_prepend(err, "qed error: ");
	}
	return NULL;
}

static bool
isprecondition(struct ast_stmt *);

static struct error *
parameterise_state(struct state *s, struct ast_function *f)
{
	/* declare params and locals in stack frame */
	struct ast_variable **param = ast_function_params(f);
	int nparams = ast_function_nparams(f);
	for (int i = 0; i < nparams; i++) {
		struct ast_variable *p = param[i];
		state_declare(s, p, true);
		if (ast_type_base(ast_variable_type(p)) == TYPE_INT) {
			struct object *obj = state_getobject(s, ast_variable_name(p));
			assert(obj);
			object_assign(obj, state_vconst(s));
		}
	}

	struct ast_block *abs = ast_function_abstract(f);
	int nstmts = ast_block_nstmts(abs);
	struct ast_stmt **stmt = ast_block_stmts(abs);
	for (int i = 0; i < nstmts; i++) {
		struct error *err = NULL;
		if (isprecondition(stmt[i])) {
			if ((err = stmt_exec(stmt[i], s))) {
				return err;
			}
		}
	}

	return NULL;
}

static bool
isprecondition(struct ast_stmt *stmt)
{
	return ast_stmt_kind(stmt) == STMT_LABELLED
		&& strcmp(ast_stmt_labelled_label(stmt), "pre") == 0;
}

/* stmt_verify */

static struct error *
stmt_v_block_verify(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_expr_verify(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_iter_verify(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_verify(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_NOP:
		return NULL;
	case STMT_COMPOUND_V:
		return stmt_v_block_verify(stmt, state);
	case STMT_EXPR:
		return stmt_expr_verify(stmt, state);
	case STMT_ITERATION:
		return stmt_iter_verify(stmt, state);
	default:
		fprintf(stderr, "cannot verify stmt: %s\n", ast_stmt_str(stmt));
		assert(false);
	}
}

static struct error *
stmt_v_block_verify(struct ast_stmt *v_block_stmt, struct state *state)
{
	struct ast_block *b = ast_stmt_as_v_block(v_block_stmt);
	assert(ast_block_ndecls(b) == 0); /* C89: declarations at beginning of function */
	int nstmts = ast_block_nstmts(b);
	struct ast_stmt **stmt = ast_block_stmts(b);
	for (int i = 0; i < nstmts; i++) {
		struct error *err = stmt_verify(stmt[i], state);
		if (err) {
			return err;
		}
	}
	return NULL;
}

/* stmt_expr_verify */

static struct error *
stmt_expr_verify(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *expr = ast_stmt_as_expr(stmt);
	if (ast_expr_decide(expr, state)) {
		return NULL;
	}
	return error_create("cannot verify");
}

static bool
iter_empty(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_iter_verify(struct ast_stmt *stmt, struct state *state)
{
	/* check for empty sets */
	if (iter_empty(stmt, state)) {
		return NULL;
	}

	/* neteffect is an iter statement */
	struct ast_stmt *body = ast_stmt_iter_body(stmt);
	assert(ast_stmt_kind(body) == STMT_COMPOUND);
	struct ast_block *block = ast_stmt_as_block(body);
	assert(ast_block_ndecls(block) == 0 && ast_block_nstmts(block) == 1);
	struct ast_expr *assertion = ast_stmt_as_expr(ast_block_stmts(block)[0]);

	/* we're currently discarding analysis of `offset` and relying on the
	 * bounds (lw, up beneath) alone */
	struct ast_expr *lw = ast_stmt_iter_lower_bound(stmt),
			*up = ast_stmt_iter_upper_bound(stmt);

	if (!ast_expr_rangedecide(assertion, lw, up, state)) {
		return error_create("could not verify");	
	}
	return NULL;
}

static bool
iter_empty(struct ast_stmt *stmt, struct state *state)
{
	struct error *err = stmt_exec(ast_stmt_iter_init(stmt), state);
	assert(!err);
	/* iter is empty if its cond is false after init (executed above) */
	return !ast_expr_decide(ast_stmt_as_expr(ast_stmt_iter_cond(stmt)), state);
}

/* stmt_exec */

static struct error *
stmt_compound_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_iter_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_jump_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_exec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_LABELLED:
		return stmt_exec(ast_stmt_labelled_stmt(stmt), state);
	case STMT_COMPOUND:
		return stmt_compound_exec(stmt, state);
	case STMT_COMPOUND_V:
		return NULL;
	case STMT_EXPR:
		return ast_expr_exec(ast_stmt_as_expr(stmt), state);
	case STMT_ITERATION:
		return stmt_iter_exec(stmt, state);
	case STMT_JUMP:
		return stmt_jump_exec(stmt, state);
	default:
		assert(false);
	}
}

/* stmt_compound_exec */

static struct error *
stmt_compound_exec(struct ast_stmt *stmt, struct state *state)
{
	struct ast_block *b = ast_stmt_as_block(stmt);
	assert(ast_block_ndecls(b) == 0);
	int nstmt = ast_block_nstmts(b);
	struct ast_stmt **stmts = ast_block_stmts(b);
	for (int i = 0; i < nstmt; i++) {
		struct error *err = stmt_exec(stmts[i], state);
		if (err) {
			return err;
		}
	}
	return NULL;
}

/* stmt_expr_eval */

static struct ast_stmt *
iter_neteffect(struct ast_stmt *);

static struct error *
stmt_iter_exec(struct ast_stmt *stmt, struct state *state)
{
	/* TODO: check internal consistency of iteration */

	struct ast_stmt *neteffect = iter_neteffect(stmt);
	if (!neteffect) {
		return NULL;
	}

	struct result *res = ast_stmt_absexec(neteffect, state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}

	ast_stmt_destroy(neteffect);

	return NULL;
}

/* iter_neteffect */

static struct ast_expr *
stmt_itereval(struct ast_stmt *stmt, struct ast_stmt *iter, struct state *state);

static struct ast_stmt *
iter_neteffect(struct ast_stmt *iter)
{
	struct ast_block *abs = ast_stmt_iter_abstract(iter);
	assert(abs);

	int nstmts = ast_block_nstmts(abs);
	if (!nstmts) {
		return NULL;
	}

	assert(ast_block_ndecls(abs) == 0 && nstmts == 1);

	return ast_stmt_create_iter(
		NULL,
		ast_stmt_copy(ast_stmt_iter_init(iter)),
		ast_stmt_copy(ast_stmt_iter_cond(iter)),
		ast_expr_copy(ast_stmt_iter_iter(iter)),
		ast_block_create(NULL, 0, NULL, 0),
		ast_stmt_create_compound(
			NULL, ast_block_copy(ast_stmt_iter_abstract(iter))
		)
	);
}

/* stmt_itereval */

static struct ast_expr *
stmt_expr_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state);

static struct ast_expr *
stmt_compound_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state);

static struct ast_expr *
stmt_sel_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state);

static struct ast_expr *
stmt_iter_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state);

static struct ast_expr *
stmt_itereval(struct ast_stmt *stmt, struct ast_stmt *iter, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_EXPR:
		return stmt_expr_itereval(stmt, iter, state);
	case STMT_COMPOUND:
		return stmt_compound_itereval(stmt, iter, state);
	case STMT_SELECTION:
		return stmt_sel_itereval(stmt, iter, state);
	case STMT_ITERATION:
		return stmt_iter_itereval(stmt, iter, state);
	default:
		assert(false);
	}
}

static struct ast_expr *
stmt_expr_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_expr *expr = ast_stmt_as_expr(stmt);
	assert(ast_stmt_kind(stmt) == STMT_ALLOCATION);
	return expr;
}

static struct ast_expr *
stmt_compound_itereval(struct ast_stmt *compound, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_block *b = ast_stmt_as_block(compound);
	assert(ast_block_ndecls(b) == 0 && ast_block_nstmts(b) == 1);
	struct ast_stmt **stmt = ast_block_stmts(b);
	assert(!ast_block_decls(b) && stmt);
	return stmt_itereval(stmt[0], iter, state);
}


/* stmt_sel_itereval */

static bool
expr_iter_decide(struct ast_expr *, struct ast_stmt *iter, struct state *state);

static struct ast_expr *
stmt_sel_itereval(struct ast_stmt *sel, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_expr *cond = ast_stmt_sel_cond(sel);
	if (expr_iter_decide(cond, iter, state)) {
		return stmt_itereval(ast_stmt_sel_body(sel), iter, state);
	}

	struct ast_stmt *nest = ast_stmt_sel_nest(sel);
	assert(nest);
	return stmt_itereval(nest, iter, state);
}

static bool
expr_unary_iter_decide(struct ast_expr *expr, struct ast_stmt *iter,
		struct state *state);

static bool
expr_assertion_iter_decide(struct ast_expr *expr, struct ast_stmt *iter,
		struct state *state);

static bool
expr_iter_decide(struct ast_expr *expr, struct ast_stmt *iter,
		struct state *state)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_UNARY:
		return expr_unary_iter_decide(expr, iter, state);
	case EXPR_ISDEALLOCAND:
		return expr_assertion_iter_decide(expr, iter, state);
	default:
		assert(false);
	}
}

static bool
expr_unary_iter_decide(struct ast_expr *expr, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_expr *operand = ast_expr_unary_operand(expr);
	switch (ast_expr_unary_op(expr)) {
	case UNARY_OP_BANG:
		return !expr_iter_decide(operand, iter, state);
	default:
		assert(false);
	}
}

static struct ast_expr *
hack_access_from_assertion(struct ast_expr *expr);

static bool
expr_assertion_iter_decide(struct ast_expr *expr, struct ast_stmt *iter,
		struct state *state)
{
	struct ast_expr *access = hack_access_from_assertion(expr);

	struct object *obj = lvalue_object(ast_expr_lvalue(access, state));
	assert(obj);

	struct ast_expr *lw = ast_stmt_iter_lower_bound(iter),
			*up = ast_stmt_iter_upper_bound(iter);

	bool deallocands = state_range_aredeallocands(state, obj, lw, up);

	object_destroy(obj);

	return deallocands;
}

static struct ast_expr *
hack_access_from_assertion(struct ast_expr *expr)
{
	struct ast_expr *assertand = ast_expr_isdeallocand_assertand(expr);
	assert(ast_expr_kind(assertand) == EXPR_UNARY
			&& ast_expr_unary_op(assertand) == UNARY_OP_DEREFERENCE);
	return assertand;
}


/* stmt_iter_itereval */

static struct ast_expr *
stmt_iter_itereval(struct ast_stmt *stmt, struct ast_stmt *iter,
		struct state *state)
{
	assert(false);
}

static struct error *
stmt_jump_exec(struct ast_stmt *stmt, struct state *state)
{
	struct result *res = ast_expr_eval(ast_stmt_jump_rv(stmt), state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}
	if (result_hasvalue(res)) {
		struct object *obj = state_getresult(state); 
		assert(obj);
		object_assign(obj, value_copy(result_as_value(res)));
		result_destroy(res);
		state_undeclarevars(state);
	}
	return NULL;
}

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state,
		struct externals *ext)
{
	struct error *err = NULL;

	/*printf("actual: %s\n", state_str(actual_state));*/
	if (!state_hasgarbage(actual_state)) {
		return error_create("garbage on heap");
	}

	struct state *alleged_state = state_create(
		dynamic_str(ast_function_name(f)), ext, ast_function_type(f)
	);
	if ((err = parameterise_state(alleged_state, f))) {
		return err;
	}

	/* mutates alleged_state */
	struct result *res = ast_function_absexec(f, alleged_state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}

	/*printf("actual: %s\n", state_str(actual_state));*/
	/*printf("alleged: %s\n", state_str(alleged_state));*/

	bool equiv = state_equal(actual_state, alleged_state);

	state_destroy(alleged_state); /* actual_state handled by caller */ 
	
	if (!equiv) {
		/* XXX: print states */
		return error_create("actual and alleged states differ");
	}

	return NULL;
}
