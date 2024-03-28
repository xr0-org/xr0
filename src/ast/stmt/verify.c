#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "lex.h"
#include "intern.h"
#include "props.h"
#include "object.h"
#include "state.h"
#include "stmt.h"
#include "util.h"
#include "value.h"

struct error *
ast_stmt_process(struct ast_stmt *stmt, char *fname, struct state *state)
{
	struct error *err;

	if (ast_stmt_kind(stmt) == STMT_COMPOUND_V) {
		if ((err = ast_stmt_verify(stmt, state))) {
			struct strbuilder *b = strbuilder_create();
			struct lexememarker *loc = ast_stmt_lexememarker(stmt); 
			assert(loc);
			char *m = lexememarker_str(loc);
			strbuilder_printf(b, "%s: %s", m, err->msg);
			free(m);
			return error_create(strbuilder_build(b));
		}
	}
	if ((err = ast_stmt_exec(stmt, state))) {
		struct strbuilder *b = strbuilder_create();
		struct lexememarker *loc = ast_stmt_lexememarker(stmt); 
		assert(loc);
		char *m = lexememarker_str(loc);
		strbuilder_printf(b, "%s:%s: cannot exec statement: %s", m, fname, err->msg);
		free(m);
		return error_create(strbuilder_build(b));
	}
	return NULL;
}

static struct preresult *
stmt_installprop(struct ast_stmt *stmt, struct state *state);

struct preresult *
ast_stmt_preprocess(struct ast_stmt *stmt, struct state *state)
{
	if (ast_stmt_isassume(stmt)) {
		return stmt_installprop(stmt, state);
	}
	return preresult_empty_create();
}

static struct preresult *
stmt_installprop(struct ast_stmt *stmt, struct state *state)
{
	return ast_expr_assume(ast_stmt_as_expr(ast_stmt_labelled_stmt(stmt)), state);
}

/* stmt_verify */

static struct error *
stmt_v_block_verify(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_expr_verify(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_iter_verify(struct ast_stmt *stmt, struct state *state);

struct error *
ast_stmt_verify(struct ast_stmt *stmt, struct state *state)
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
		struct error *err = ast_stmt_verify(stmt[i], state);
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
	return error_create("cannot verify statement");
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
	struct error *err = ast_stmt_exec(ast_stmt_iter_init(stmt), state);
	assert(!err);
	/* iter is empty if its cond is false after init (executed above) */
	return !ast_expr_decide(ast_stmt_as_expr(ast_stmt_iter_cond(stmt)), state);
}


/* stmt_exec */

static struct error *
stmt_compound_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_sel_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_iter_exec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_jump_exec(struct ast_stmt *stmt, struct state *state);

struct error *
ast_stmt_exec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_NOP:
		return NULL;
	case STMT_LABELLED:
		return ast_stmt_exec(ast_stmt_labelled_stmt(stmt), state);
	case STMT_COMPOUND:
		return stmt_compound_exec(stmt, state);
	case STMT_COMPOUND_V:
		return NULL;
	case STMT_EXPR:
		return ast_expr_exec(ast_stmt_as_expr(stmt), state);
	case STMT_SELECTION:
		return stmt_sel_exec(stmt, state);
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
		struct error *err = ast_stmt_exec(stmts[i], state);
		if (err) {
			return err;
		}
		if (ast_stmt_isterminal(stmts[i], state)) {
			break;
		}
	}
	return NULL;
}

/* stmt_sel_exec */

static struct error *
stmt_sel_exec(struct ast_stmt *stmt, struct state *state)
{
	struct decision dec = sel_decide(ast_stmt_sel_cond(stmt), state);
	if (dec.err) {
		return dec.err;
	}
	if (dec.decision) {
		return ast_stmt_exec(ast_stmt_sel_body(stmt), state);
	}
	assert(!ast_stmt_sel_nest(stmt));
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

	struct result *res = ast_stmt_absexec(neteffect, state, true);
	if (result_iserror(res)) {
		return result_as_error(res);
	}

	ast_stmt_destroy(neteffect);

	return NULL;
}

/* iter_neteffect */

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

static struct error *
stmt_jump_exec(struct ast_stmt *stmt, struct state *state)
{
	/* TODO: install propositions corresponding to dereferencability */

	struct result *res = ast_expr_eval(ast_stmt_jump_rv(stmt), state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}
	if (result_hasvalue(res)) {
		struct object *obj = state_getresult(state); 
		assert(obj);
		object_assign(obj, value_copy(result_as_value(res)));
		/* destroy result if exists */
		
	}
	return NULL;
}

static struct result *
labelled_absexec(struct ast_stmt *stmt, struct state *state, bool should_setup);

static struct result *
sel_absexec(struct ast_stmt *stmt, struct state *state, bool should_setup);

static struct result *
iter_absexec(struct ast_stmt *stmt, struct state *state);

static struct result *
comp_absexec(struct ast_stmt *stmt, struct state *state, bool should_setup);

static struct result *
jump_absexec(struct ast_stmt *, struct state *);

struct result *
ast_stmt_absexec(struct ast_stmt *stmt, struct state *state, bool should_setup)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_NOP:
		return result_value_create(NULL);
	case STMT_LABELLED:
		return labelled_absexec(stmt, state, should_setup);
	case STMT_EXPR:
		return ast_expr_absexec(ast_stmt_as_expr(stmt), state);
	case STMT_SELECTION:
		return sel_absexec(stmt, state, should_setup);
	case STMT_ITERATION:
		return iter_absexec(stmt, state);
	case STMT_COMPOUND:
		return comp_absexec(stmt, state, should_setup);
	case STMT_JUMP:
		return jump_absexec(stmt, state);
	default:
		assert(false);
	}
}

static struct result *
labelled_absexec(struct ast_stmt *stmt, struct state *state, bool should_setup)
{
	if (!ast_stmt_ispre(stmt)) {
		assert(false);
	}
	struct ast_stmt *setup = ast_stmt_labelled_stmt(stmt);
	if (!setup) {
		assert(false);
	}
	if (!should_setup) {
		/* if abstract is called we don't execute setup */
		return result_value_create(NULL);
	}
	return ast_stmt_absexec(setup, state, should_setup);
}

static struct result *
sel_absexec(struct ast_stmt *stmt, struct state *state, bool should_setup)
{
	struct decision dec = sel_decide(ast_stmt_sel_cond(stmt), state);
	if (dec.err) {
		return result_error_create(dec.err);
	}
	if (dec.decision) {
		return ast_stmt_absexec(ast_stmt_sel_body(stmt), state, should_setup);
	}
	assert(!ast_stmt_sel_nest(stmt));
	return result_value_create(NULL);
}

struct decision
sel_decide(struct ast_expr *control, struct state *state)
{
	struct result *res = ast_expr_pf_reduce(control, state);
	if (result_iserror(res)) {
		return (struct decision) { .err = result_as_error(res) };
	}
	assert(result_hasvalue(res)); /* TODO: user error */

	struct value *v = result_as_value(res);
	if (value_issync(v)) {
		struct ast_expr *sync = value_as_sync(v);
		struct props *p = state_getprops(state);
		if (props_get(p, sync)) {
			return (struct decision) { .decision = true, .err = NULL };
		} else if (props_contradicts(p, sync)) {
			return (struct decision) { .decision = false, .err = NULL };
		}
	}
	if (value_isconstant(v)) {
		if (value_as_constant(v)) {
			return (struct decision) { .decision = true, .err = NULL };	
		}
		return (struct decision) { .decision = false, .err = NULL };
	} 

	struct value *zero = value_int_create(0);

	if (!values_comparable(zero, v)) {
		struct strbuilder *b = strbuilder_create();
		char *c_str = ast_expr_str(control);
		char *v_str = value_str(v);
		strbuilder_printf(
			b, "`%s' with value `%s' is undecidable",
			c_str, v_str
		);
		free(v_str);
		free(c_str);
		return (struct decision) {
			.decision = false,
			.err      = error_create(strbuilder_build(b)),
		};
	}

	bool nonzero = !value_equal(zero, v);
	value_destroy(zero);
	return (struct decision) { .decision = nonzero, .err = NULL };
}

static struct ast_expr *
hack_alloc_from_neteffect(struct ast_stmt *);

static struct result *
iter_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct error *err;

	struct ast_expr *alloc = hack_alloc_from_neteffect(stmt),
			*lw = ast_stmt_iter_lower_bound(stmt),
			*up = ast_stmt_iter_upper_bound(stmt);

	if ((err = ast_expr_alloc_rangeprocess(alloc, lw, up, state))) {
		return result_error_create(err);
	}
	return result_value_create(NULL);
}

static struct ast_expr *
hack_alloc_from_neteffect(struct ast_stmt *stmt)
{
	struct ast_stmt *body = ast_stmt_iter_body(stmt);
	assert(ast_stmt_kind(body) == STMT_COMPOUND);
	struct ast_block *block = ast_stmt_as_block(body);
	assert(ast_block_ndecls(block) == 0 && ast_block_nstmts(block) == 1);
	return ast_stmt_as_expr(ast_block_stmts(block)[0]);
}

static struct result *
comp_absexec(struct ast_stmt *stmt, struct state *state, bool should_setup)
{
	struct ast_block *b = ast_stmt_as_block(stmt);
	struct ast_stmt **stmts = ast_block_stmts(b);
	for (int i = 0; i < ast_block_nstmts(b); i++) {
		struct result *res = ast_stmt_absexec(stmts[i], state, should_setup);
		if (result_iserror(res)) {
			return res;
		}
	}
	return result_value_create(NULL);
}

static struct result *
jump_absexec(struct ast_stmt *stmt, struct state *state)
{
	return ast_expr_absexec(
		ast_expr_assignment_create(
			ast_expr_identifier_create("return"),
			ast_stmt_jump_rv(stmt)
		), 
		state
	);
}

static struct error *
stmt_setupabsexec(struct ast_stmt *, struct state *);

static struct error *
labelled_setupabsexec(struct ast_stmt *, struct state *);

static struct error *
sel_setupabsexec(struct ast_stmt *, struct state *);

static struct error *
comp_setupabsexec(struct ast_stmt *, struct state *);

struct error *
ast_stmt_setupabsexec(struct ast_stmt *stmt, struct state *state)
{
	if (ast_stmt_kind(stmt) != STMT_SELECTION) {
		return NULL;
	}
	return stmt_setupabsexec(stmt, state);
}

static struct error *
stmt_setupabsexec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {	
	case STMT_EXPR:
	case STMT_ALLOCATION:
	case STMT_JUMP:
		return NULL;
	case STMT_LABELLED:
		return labelled_setupabsexec(stmt, state);
	case STMT_SELECTION:
		return sel_setupabsexec(stmt, state);
	case STMT_COMPOUND:
		return comp_setupabsexec(stmt, state);
	default:
		assert(false);
	}
}

static struct error *
labelled_setupabsexec(struct ast_stmt *stmt, struct state *state)
{
	/* XXX: dedupe the execution of setups */
	struct result *res = ast_stmt_absexec(stmt, state, true);
	if (result_iserror(res)) {
		return result_as_error(res);
	}
	return NULL;
}

static struct error *
sel_setupabsexec(struct ast_stmt *stmt, struct state *state)
{
	struct decision dec = sel_decide(ast_stmt_sel_cond(stmt), state);
	if (dec.err) {
		return dec.err;
	}
	if (dec.decision) {
		return stmt_setupabsexec(ast_stmt_sel_body(stmt), state);
	}
	assert(!ast_stmt_sel_nest(stmt));
	return NULL;
}

static struct error *
comp_setupabsexec(struct ast_stmt *stmt, struct state *state)
{
	struct error *err;
	struct ast_block *b = ast_stmt_as_block(stmt);
	assert(ast_block_ndecls(b) == 0);
	int nstmt = ast_block_nstmts(b);
	struct ast_stmt **stmts = ast_block_stmts(b);
	for (int i = 0; i < nstmt; i++) {
		if (ast_stmt_ispre(stmts[i])) {
			if ((err = stmt_setupabsexec(stmts[i], state))) {
				return err;
			}
			if (ast_stmt_isterminal(stmts[i], state)) {
				break;
			}
		}
	}
	return NULL;
}
