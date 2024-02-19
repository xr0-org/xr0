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
ast_stmt_process(struct ast_stmt *stmt, struct state *state)
{
	struct error *err = NULL;

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
		strbuilder_printf(b, "%s: cannot exec statement: %s", m, err->msg);
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
	if (ast_stmt_ispre(stmt)) {
		struct error *err = ast_stmt_exec(stmt, state);
		if (err) {
			return preresult_error_create(err);
		}
	} else if (ast_stmt_isassume(stmt)) {
		printf("assume: %s\n", ast_stmt_str(stmt));
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

	struct result *res = ast_stmt_absexec(neteffect, state);
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
	struct result *res = ast_expr_eval(ast_stmt_jump_rv(stmt), state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}
	if (result_hasvalue(res)) {
		struct object *obj = state_getresult(state); 
		assert(obj);
		object_assign(obj, value_copy(result_as_value(res)));
		result_destroy(res);
	}
	return NULL;
}

static struct result *
sel_absexec(struct ast_stmt *stmt, struct state *state);

static struct result *
iter_absexec(struct ast_stmt *stmt, struct state *state);

static struct result *
comp_absexec(struct ast_stmt *stmt, struct state *state);

static struct result *
alloc_absexec(struct ast_stmt *stmt, struct state *state);

struct result *
ast_stmt_absexec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_LABELLED:
		/* labelled statements are verified not executed when we
		 * transitively call a function */
		return result_value_create(NULL);
	case STMT_EXPR:
		return ast_expr_absexec(ast_stmt_as_expr(stmt), state);
	case STMT_SELECTION:
		return sel_absexec(stmt, state);
	case STMT_ITERATION:
		return iter_absexec(stmt, state);
	case STMT_COMPOUND:
		return comp_absexec(stmt, state);
	case STMT_ALLOCATION:
		return alloc_absexec(stmt, state);
	default:
		assert(false);
	}
}

static struct ast_stmt *
hack_alloc_from_neteffect(struct ast_stmt *);

static struct object *
hack_base_object_from_alloc(struct ast_stmt *, struct state *);

static struct result *
iter_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct error *err;

	struct ast_stmt *alloc = hack_alloc_from_neteffect(stmt);

	struct object *obj = hack_base_object_from_alloc(alloc, state);

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

	/*printf("stmt: %s\n", ast_stmt_str(stmt));*/
	/*printf("state: %s\n", state_str(state));*/

	struct ast_expr *res_lw = value_to_expr(result_as_value(result_lw)),
			*res_up = value_to_expr(result_as_value(result_up));

	result_destroy(result_up);
	result_destroy(result_lw);

	if (ast_stmt_alloc_isalloc(alloc)) {
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

static struct ast_stmt *
hack_alloc_from_neteffect(struct ast_stmt *stmt)
{
	struct ast_stmt *body = ast_stmt_iter_body(stmt);
	assert(ast_stmt_kind(body) == STMT_COMPOUND);
	struct ast_block *block = ast_stmt_as_block(body);
	assert(ast_block_ndecls(block) == 0 && ast_block_nstmts(block) == 1);
	return ast_block_stmts(block)[0];
}

static struct object *
hack_base_object_from_alloc(struct ast_stmt *alloc, struct state *state)
{
	/* we're currently discarding analysis of `offset` and relying on the
	 * bounds (lower, upper beneath) alone */
	struct ast_expr *acc = ast_stmt_alloc_arg(alloc); /* `*(arr+offset)` */
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
sel_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct decision dec = sel_decide(ast_stmt_sel_cond(stmt), state);
	if (dec.err) {
		return result_error_create(dec.err);
	}
	if (dec.decision) {
		return ast_stmt_absexec(ast_stmt_sel_body(stmt), state);
	}
	assert(!ast_stmt_sel_nest(stmt));
	return result_value_create(NULL);
}

struct decision
sel_decide(struct ast_expr *control, struct state *state)
{
	/*printf("(sel_decide) state: %s\n", state_str(state));*/
	/*printf("(sel_decide) control: %s\n", ast_expr_str(control));*/
	struct result *res = ast_expr_pf_reduce(control, state);
	if (result_iserror(res)) {
		return (struct decision) { .err = result_as_error(res) };
	}
	assert(result_hasvalue(res)); /* TODO: user error */

	struct value *v = result_as_value(res);
	/*printf("(sel_decide) value: %s\n", value_str(v));*/
	if (value_issync(v)) {
		struct ast_expr *sync = value_as_sync(v);
		/*printf("state: %s\n", state_str(state));*/
		/*printf("sync: %s\n", ast_expr_str(sync));*/
		struct props *p = state_getprops(state);
		if (props_get(p, sync)) {
			return (struct decision) { .decision = true, .err = NULL };
		} else if (props_contradicts(p, sync)) {
			return (struct decision) { .decision = false, .err = NULL };
		}

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

static struct result *
alloc_process(struct ast_stmt *, struct state *);

static struct result *
alloc_absexec(struct ast_stmt *alloc, struct state *state)
{
	struct result *res = alloc_process(alloc, state);
	if (result_iserror(res)) {
		return res;
	}
	if (result_hasvalue(res)) {
		struct object *obj = lvalue_object(
			ast_expr_lvalue(ast_stmt_alloc_arg(alloc), state)
		);
		assert(obj);
		object_assign(obj, value_copy(result_as_value(res)));
	}
	return res;
}

/* operates at location level. It either creates an object on the heap and returns
 * a location or gets the location pointed to by an lvalue and attempts to free
 * possibly returning an error
 * */
static struct result *
alloc_process(struct ast_stmt *alloc, struct state *state)
{
	if (ast_stmt_alloc_isalloc(alloc)) {
		/* TODO: size needs to be passed in here when added to .alloc */
		return result_value_create(state_alloc(state));
	}

	/* dealloc */
	struct ast_expr *arg = ast_stmt_alloc_arg(alloc);
	/* arg is pointing at the heap location we want to free, so we want its
	 * value rather than location */
	struct result *res = ast_expr_eval(arg, state);
	if (result_iserror(res)) {
		return res;
	}
	struct value *val = result_as_value(res);
	assert(val);
	struct error *err = state_dealloc(state, val);
	if (err) {
		return result_error_create(err);
	}
	value_destroy(val);
	return result_value_create(NULL);
}

static struct error *
ast_stmt_compound_precondsverify(struct ast_stmt *, struct state *);

struct error *
ast_stmt_precondsverify(struct ast_stmt *stmt, struct state *s)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_EXPR:
		return ast_expr_precondsverify(ast_stmt_as_expr(stmt), s);
	case STMT_COMPOUND:
		return ast_stmt_compound_precondsverify(stmt, s);
	default:
		assert(false);
	}
}

static struct error *
ast_stmt_compound_precondsverify(struct ast_stmt *stmt, struct state *s)
{
	struct error *err;

	struct ast_block *b = ast_stmt_as_block(stmt);
	int n = ast_block_nstmts(b);
	struct ast_stmt **stmts = ast_block_stmts(b);

	for (int i = 0; i < n; i++) {
		if ((err = ast_stmt_precondsverify(stmts[i], s))) {
			return err;
		}
	}
	return NULL;
}
