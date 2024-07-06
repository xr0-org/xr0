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

static struct error *
ast_stmt_linearise_proper(struct ast_stmt *, struct ast_block *, struct lexememarker *,
		struct state *);

struct error *
ast_stmt_linearise(struct ast_stmt *stmt, struct state *state)
{
	struct lexememarker *loc = ast_stmt_lexememarker(stmt);
	struct ast_block *b = ast_block_create(NULL, 0);
	struct error *err = ast_stmt_linearise_proper(
		stmt, b, lexememarker_copy(loc), state
	);
	if (err) {
		return err;
	}
	struct frame *inter_frame = frame_intermediate_create(
		dynamic_str("inter"), b, state_execmode(state)
	);
	state_pushframe(state, inter_frame);
	return NULL;
}

static struct error *
expr_linearise(struct ast_stmt *, struct ast_block *, struct lexememarker *,
		struct state *);

static struct error *
jump_linearise(struct ast_stmt *, struct ast_block *, struct lexememarker *,
		struct state *);

static struct error *
selection_linearise(struct ast_stmt *, struct ast_block *, struct lexememarker *,
		struct state *);

static struct error *
ast_stmt_linearise_proper(struct ast_stmt *stmt, struct ast_block *b,
		struct lexememarker *loc, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_EXPR:
		return expr_linearise(stmt, b, loc, state);
	case STMT_JUMP:
		return jump_linearise(stmt, b, loc, state);
	case STMT_SELECTION:
		return selection_linearise(stmt, b, loc, state);
	default:
		assert(false);
	}
}

static struct error *
expr_linearise(struct ast_stmt *stmt, struct ast_block *b,
		struct lexememarker *loc, struct state *state)
{
	struct ast_expr *expr = ast_expr_geninstr(
		ast_expr_copy(ast_stmt_as_expr(stmt)),
		lexememarker_copy(loc),
		b,
		state
	);
	if (expr) {
		ast_expr_destroy(expr);
	}
	return NULL;
}

static struct error *
jump_linearise(struct ast_stmt *stmt, struct ast_block *b, struct lexememarker *loc,
		struct state *state)
{
	struct ast_expr *rv = ast_stmt_jump_rv(stmt);

	struct ast_expr *gen = ast_expr_geninstr(
		rv, lexememarker_copy(loc), b, state
	);
	struct ast_stmt *newjump = ast_stmt_create_jump(
		lexememarker_copy(loc), JUMP_RETURN, gen
	);
	ast_block_append_stmt(b, newjump);
	return NULL;
}

static struct error *
selection_linearise(struct ast_stmt *stmt, struct ast_block *b, struct lexememarker *loc,
		struct state *state)
{
	struct ast_expr *cond = ast_stmt_sel_cond(stmt);
	struct ast_stmt *body = ast_stmt_sel_body(stmt),
			*nest = ast_stmt_sel_nest(stmt);
	
	struct ast_expr *newcond = ast_expr_geninstr(
		cond, lexememarker_copy(loc), b, state
	);
	struct ast_stmt *newsel = ast_stmt_create_sel(
		lexememarker_copy(loc),
		false,
		newcond,
		ast_stmt_copy(body),
		nest ? ast_stmt_copy(nest) : NULL
	);
	ast_block_append_stmt(b, newsel);
	return NULL;
}

struct error *
ast_stmt_process(struct ast_stmt *stmt, struct state *state)
{
	struct error *err;

	if (ast_stmt_ispre(stmt)) {
		return NULL;
	}
	if ((err = ast_stmt_exec(stmt, state))) {
		return err;
	}
	return NULL;
}

/* stmt_verify */

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
	case STMT_REGISTER:
		return ast_stmt_exec(stmt, state);
	case STMT_EXPR:
		return stmt_expr_verify(stmt, state);
	case STMT_ITERATION:
		return stmt_iter_verify(stmt, state);
	default:
		assert(false);
	}
}

/* stmt_expr_verify */

static struct error *
stmt_expr_verify(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *expr = ast_stmt_as_expr(stmt);
	if (ast_expr_decide(expr, state)) {
		return NULL;
	}
	return error_printf("cannot verify statement");
}

static bool
iter_empty(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_iter_verify(struct ast_stmt *stmt, struct state *state)
{
	assert(false);

	/* check for empty sets */
	if (iter_empty(stmt, state)) {
		return NULL;
	}

	/* neteffect is an iter statement */
	struct ast_stmt *body = ast_stmt_iter_body(stmt);
	assert(ast_stmt_kind(body) == STMT_COMPOUND);
	struct ast_block *b = ast_stmt_as_block(body);
	assert(ast_block_nstmts(b) == 1 && !ast_stmt_isdecl(ast_block_stmts(b)[0]));
	struct ast_expr *assertion = ast_stmt_as_expr(ast_block_stmts(b)[0]);

	/* we're currently discarding analysis of `offset` and relying on the
	 * bounds (lw, up beneath) alone */
	struct ast_expr *lw = ast_stmt_iter_lower_bound(stmt),
			*up = ast_stmt_iter_upper_bound(stmt);

	if (!ast_expr_rangedecide(assertion, lw, up, state)) {
		return error_printf("could not verify");	
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
stmt_decl_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_compoundv_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_compound_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_sel_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_iter_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_jump_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_register_exec(struct ast_stmt *, struct state *);

struct error *
ast_stmt_exec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_DECLARATION:
		return stmt_decl_exec(stmt, state);
	case STMT_NOP:
		return NULL;
	case STMT_LABELLED:
		assert(false);
	case STMT_COMPOUND:
		return stmt_compound_exec(stmt, state);
	case STMT_COMPOUND_V:
		return stmt_compoundv_exec(stmt, state);
	case STMT_EXPR:
		return ast_expr_exec(ast_stmt_as_expr(stmt), state);
	case STMT_SELECTION:
		return stmt_sel_exec(stmt, state);
	case STMT_ITERATION:
		return stmt_iter_exec(stmt, state);
	case STMT_JUMP:
		return stmt_jump_exec(stmt, state);
	case STMT_REGISTER:
		return stmt_register_exec(stmt, state);
	default:
		assert(false);
	}
}

static struct error *
decl_init(struct ast_variable *, struct state *);

static struct error *
stmt_decl_exec(struct ast_stmt *stmt, struct state *state)
{
	/* TODO: add initialisation */
	struct ast_variable_arr *vars = ast_stmt_declaration_vars(stmt);
	for (int i = 0; i < ast_variable_arr_n(vars); i++) {
		struct ast_variable *v = ast_variable_arr_v(vars)[i];
		state_declare(state, v, false);
		decl_init(v, state);	
	}
	return NULL;
}

static struct error *
decl_init(struct ast_variable *v, struct state *s)
{
	if (ast_variable_init(v)) {
		struct ast_expr *assign = ast_expr_assignment_create(
			ast_expr_identifier_create(ast_variable_name(v)),
			ast_variable_init(v)
		);
		return ast_expr_exec(assign, s);
	}
	return NULL;
}

static struct error *
stmt_compoundv_exec(struct ast_stmt *stmt, struct state *state)
{
	struct frame *block_frame = frame_block_create(
		dynamic_str("verification block"),
		ast_stmt_as_block(stmt),
		EXEC_VERIFY
	);
	state_pushframe(state, block_frame);
	return NULL;
}

static struct error *
stmt_compound_exec(struct ast_stmt *stmt, struct state *state)
{
	struct frame *block_frame = frame_block_create(
		dynamic_str("block"),
		ast_stmt_as_block(stmt),
		EXEC_ACTUAL
	);
	state_pushframe(state, block_frame);
	return NULL;
}

/* stmt_sel_exec */

static struct error *
stmt_sel_exec(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *cond = ast_stmt_sel_cond(stmt);
	struct ast_stmt *body = ast_stmt_sel_body(stmt),
			*nest = ast_stmt_sel_nest(stmt);

	struct decision dec = sel_decide(cond, state);
	if (dec.err) {
		return dec.err;
	}
	if (dec.decision) {
		return ast_stmt_exec(body, state);
	} else if (nest) {
		return ast_stmt_exec(nest, state);
	}
	return NULL;
}

/* stmt_expr_eval */

static struct ast_stmt *
iter_neteffect(struct ast_stmt *);

static struct error *
ast_stmt_absexec(struct ast_stmt *stmt, struct state *state);

static struct error *
stmt_iter_exec(struct ast_stmt *stmt, struct state *state)
{
	/* TODO: check internal consistency of iteration */

	struct ast_stmt *neteffect = iter_neteffect(stmt);
	if (!neteffect) {
		return NULL;
	}

	struct error *err = ast_stmt_absexec(neteffect, state);
	if (err) {
		return err;
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

	assert(nstmts == 1 && !ast_stmt_isdecl(ast_block_stmts(abs)[0]));

	return ast_stmt_create_iter(
		NULL,
		ast_stmt_copy(ast_stmt_iter_init(iter)),
		ast_stmt_copy(ast_stmt_iter_cond(iter)),
		ast_expr_copy(ast_stmt_iter_iter(iter)),
		ast_block_create(NULL, 0),
		ast_stmt_create_compound(
			NULL, ast_block_copy(ast_stmt_iter_abstract(iter))
		)
	);
}

static struct error *
stmt_jump_exec(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *rv = ast_stmt_jump_rv(stmt);

	if (rv) {
		struct e_res *res = ast_expr_eval(rv, state);
		if (e_res_iserror(res)) {
			return e_res_as_error(res);
		}
		if (e_res_haseval(res)) {
			struct value *v = value_copy(
				value_res_as_value(
					eval_to_value(e_res_as_eval(res), state)
				)
			);
			state_writeregister(state, v);
		}
	}
	return error_return();
}

static struct error *
register_call_exec(struct ast_expr *call, struct state *);

static struct error *
register_mov_exec(struct ast_variable *temp, struct state *);

static struct error *
stmt_register_exec(struct ast_stmt *stmt, struct state *state)
{
	/* XXX: assert we are in intermediate frame */
	if (ast_stmt_register_iscall(stmt)) {
		return register_call_exec(ast_stmt_register_call(stmt), state);
	} else {
		return register_mov_exec(ast_stmt_register_mov(stmt), state);
	}
}

static struct error *
register_call_exec(struct ast_expr *call, struct state *state)
{
	struct e_res *res = ast_expr_abseval(call, state);
	if (e_res_iserror(res)) {
		return e_res_as_error(res);
	}
	return NULL;
}

static struct e_res *
hack_default_values(struct state *);

static struct error *
register_mov_exec(struct ast_variable *temp, struct state *state)
{
	state_declare(state, temp, false);
	struct ast_expr *name = ast_expr_identifier_create(
		dynamic_str(ast_variable_name(temp))
	);
	struct e_res *l_res = ast_expr_eval(name, state);
	if (e_res_iserror(l_res)) {
		return e_res_as_error(l_res);
	}
	struct object *obj = object_res_as_object(
		state_get(state, eval_as_lval(e_res_as_eval(l_res)), true)
	);

	struct e_res *r_res = hack_default_values(state);
	if (e_res_iserror(r_res)) {
		return e_res_as_error(r_res);
	}
	if (e_res_haseval(r_res)) {
		object_assign(obj, eval_as_rval(e_res_as_eval(r_res)));
	}
	return NULL;
}

static struct e_res *
hack_default_values(struct state *state)
{
	struct ast_expr *expr = state_framecall(state);
	struct value *v = state_popregister(state);
	if (!v) {
		return e_res_empty_create();
	}
	return ast_expr_pf_augment(v, expr, state);
}

struct error *
ast_stmt_absprocess(struct ast_stmt *stmt, struct state *state)
{
	/* XXX: reject undefined things for this */
	return ast_stmt_absexec(stmt, state);
}

static struct error *
ast_stmt_absexecnosetup(struct ast_stmt *, struct state *);

struct error *
ast_stmt_absprocess_nosetup(struct ast_stmt *stmt, struct state *state)
{
	/* XXX: reject undefined things for this */
	return ast_stmt_absexecnosetup(stmt, state);
}

static struct error *
ast_stmt_absexecnosetup(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_LABELLED:
		return NULL;
	case STMT_DECLARATION:
	case STMT_NOP:
	case STMT_EXPR:
	case STMT_SELECTION:
	case STMT_ITERATION:
	case STMT_COMPOUND:
	case STMT_JUMP:
	case STMT_REGISTER:
		return ast_stmt_absexec(stmt, state);
	default:
		assert(false);
	}
}

static struct error *
labelled_absexec(struct ast_stmt *, struct state *);

static struct error *
expr_absexec(struct ast_expr *, struct state *);

static struct error *
sel_absexec(struct ast_stmt *stmt, struct state *);

static struct error *
iter_absexec(struct ast_stmt *stmt, struct state *);

static struct error *
comp_absexec(struct ast_stmt *stmt, struct state *);

static struct error *
jump_absexec(struct ast_stmt *, struct state *);

static struct error *
ast_stmt_absexec(struct ast_stmt *stmt, struct state *state)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_DECLARATION:
		return stmt_decl_exec(stmt, state);
	case STMT_NOP:
		return NULL;
	case STMT_LABELLED:
		return labelled_absexec(stmt, state);
	case STMT_EXPR:
		return expr_absexec(ast_stmt_as_expr(stmt), state);
	case STMT_SELECTION:
		return sel_absexec(stmt, state);
	case STMT_ITERATION:
		return iter_absexec(stmt, state);
	case STMT_COMPOUND:
		return comp_absexec(stmt, state);
	case STMT_JUMP:
		return jump_absexec(stmt, state);
	case STMT_REGISTER:
		return stmt_register_exec(stmt, state);
	default:
		assert(false);
	}
}

static struct error *
labelled_absexec(struct ast_stmt *stmt, struct state *state)
{
	assert(ast_stmt_ispre(stmt));
	struct ast_stmt *inner = ast_stmt_labelled_stmt(stmt);
	if (ast_stmt_kind(inner) == STMT_SELECTION) {
		return error_printf("setup preconditions must be decidable");
	}
	struct ast_block *b = ast_stmt_labelled_as_block(stmt);	
	struct frame *setup_frame = frame_setup_create(
		dynamic_str("setup"),
		b,
		state_next_execmode(state)
	);
	state_pushframe(state, setup_frame);
	return NULL;
}

static struct error *
expr_absexec(struct ast_expr *expr, struct state *state)
{
	struct e_res *res = ast_expr_abseval(expr, state);
	if (e_res_iserror(res)) {
		return e_res_as_error(res);
	}
	return NULL;
}

static struct error *
sel_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *cond = ast_stmt_sel_cond(stmt);
	struct ast_stmt *body = ast_stmt_sel_body(stmt),
			*nest = ast_stmt_sel_nest(stmt);
	struct decision dec = sel_decide(cond, state);
	if (dec.err) {
		return dec.err;
	}
	if (dec.decision) {
		return ast_stmt_absexec(body, state);
	} else if (nest) {
		return ast_stmt_absexec(nest, state);
	}
	return NULL;
}

struct decision
sel_decide(struct ast_expr *control, struct state *state)
{
	struct e_res *res = ast_expr_pf_reduce(control, state);
	if (e_res_iserror(res)) {
		return (struct decision) { .err = e_res_as_error(res) };
	}
	assert(e_res_haseval(res)); /* TODO: user error */

	struct value *v = value_res_as_value(
		eval_to_value(e_res_as_eval(res), state)
	);
	struct bool_res *dec_res = value_decide(v, state);
	if (bool_res_iserror(dec_res)) {
		return (struct decision) { .err = bool_res_as_error(dec_res) };
	}
	return (struct decision) {
		.decision	= bool_res_as_bool(dec_res),
		.err		= NULL,
	};
}

static struct ast_expr *
hack_alloc_from_neteffect(struct ast_stmt *);

static struct error *
iter_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct error *err;

	struct ast_expr *alloc = hack_alloc_from_neteffect(stmt),
			*lw = ast_stmt_iter_lower_bound(stmt),
			*up = ast_stmt_iter_upper_bound(stmt);

	if ((err = ast_expr_alloc_rangeprocess(alloc, lw, up, state))) {
		return err;
	}
	return NULL; }

static struct ast_expr *
hack_alloc_from_neteffect(struct ast_stmt *stmt)
{
	struct ast_stmt *body = ast_stmt_iter_body(stmt);
	assert(ast_stmt_kind(body) == STMT_COMPOUND);
	struct ast_block *block = ast_stmt_as_block(body);
	assert(ast_block_nstmts(block) == 1 && !ast_stmt_isdecl(ast_block_stmts(block)[0]));
	return ast_stmt_as_expr(ast_block_stmts(block)[0]);
}

static struct error *
comp_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct frame *block_frame = frame_block_create(
		dynamic_str("block"),
		ast_stmt_as_block(stmt),
		state_next_execmode(state)
	);
	state_pushframe(state, block_frame);
	return NULL;
}

static struct error *
jump_absexec(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *rv = ast_stmt_jump_rv(stmt);

	if (rv) {
		struct e_res *res = ast_expr_abseval(rv, state);
		if (e_res_iserror(res)) {
			return e_res_as_error(res);
		}
		if (e_res_haseval(res)) {
			struct value *v = value_copy(
				value_res_as_value(
					eval_to_value(e_res_as_eval(res), state)
				)
			);
			state_writeregister(state, v);
		}
	}
	return error_return();
}

static struct error *
labelled_buildsetup(struct ast_stmt *, struct state *, struct ast_block *);

static struct error *
sel_buildsetup(struct ast_stmt *, struct state *, struct ast_block *);

static struct error *
comp_buildsetup(struct ast_stmt *, struct state *, struct ast_block *);

struct error *
ast_stmt_buildsetup(struct ast_stmt *stmt, struct state *state, struct ast_block *setups) {
	switch (ast_stmt_kind(stmt)) {	
	case STMT_DECLARATION:
	case STMT_NOP:
	case STMT_JUMP:
	case STMT_REGISTER:
	case STMT_EXPR:
	case STMT_ITERATION:
		return NULL;
	case STMT_LABELLED:
		return labelled_buildsetup(stmt, state, setups);
	case STMT_SELECTION:
		return sel_buildsetup(stmt, state, setups);
	case STMT_COMPOUND:
		return comp_buildsetup(stmt, state, setups);
	default:
		assert(false);
	}
}

static struct error *
labelled_buildsetup(struct ast_stmt *stmt, struct state *state, struct ast_block *setups)
{
	struct ast_block *b = ast_stmt_labelled_as_block(stmt);
	/* XXX: assert no declarations */
	int nstmts = ast_block_nstmts(b);
	struct ast_stmt **stmts = ast_block_stmts(b);
	for (int i = 0; i < nstmts; i++) {
		if (ast_stmt_kind(stmts[i]) == STMT_SELECTION) {
			return error_printf("setup preconditions must be decidable");
		}
		ast_block_append_stmt(setups, ast_stmt_copy(stmts[i]));	
	}
	return NULL;
}

static struct error *
sel_buildsetup(struct ast_stmt *stmt, struct state *state, struct ast_block *setups)
{
	struct ast_expr *cond = ast_stmt_sel_cond(stmt);
	struct ast_stmt *body = ast_stmt_sel_body(stmt),
			*nest = ast_stmt_sel_nest(stmt);
	struct decision dec = sel_decide(cond, state);
	if (dec.err) {
		// v_printf("setup branch decision error: %s\n", error_str(dec.err));
		/* XXX: if error is `undecidable', must be decidable in some other branch */
		return NULL;
	}
	if (dec.decision) {
		return ast_stmt_buildsetup(body, state, setups);
	} else if (nest) {
		return ast_stmt_buildsetup(nest, state, setups);
	}
	return NULL;
}

static struct error *
comp_buildsetup(struct ast_stmt *stmt, struct state *state, struct ast_block *setups)
{
	struct ast_block *b = ast_stmt_as_block(stmt);
	int nstmts = ast_block_nstmts(b);
	struct ast_stmt **stmts = ast_block_stmts(b);
	for (int i = 0; i < nstmts; i++) {
		struct error *err = ast_stmt_buildsetup(stmts[i], state, setups);
		if (err) {
			return err;
		}
	}	
	return NULL;
}
