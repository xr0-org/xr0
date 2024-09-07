#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "ext.h"
#include "expr.h"
#include "lex.h"
#include "intern.h"
#include "object.h"
#include "state.h"
#include "stmt.h"
#include "util.h"
#include "value.h"

#include "type.h"

static struct error *
linearise_proper(struct ast_stmt *, struct ast_block *, struct lexememarker *,
		struct state *);

static struct error *
linearise(struct ast_stmt *stmt, struct state *state)
{
	struct lexememarker *loc = ast_stmt_lexememarker(stmt);
	struct ast_block *b = ast_block_create(NULL, 0);
	struct error *err = linearise_proper(
		stmt, b, lexememarker_copy(loc), state
	);
	if (err) {
		return err;
	}
	struct frame *inter_frame = frame_linear_create(
		dynamic_str("inter"), b, state
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
linearise_proper(struct ast_stmt *stmt, struct ast_block *b,
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
	if (expr) { /* XXX: ensure all cases return an expression */
		ast_block_append_stmt(b, ast_stmt_create_expr(loc, expr));
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

/* stmt_verify */
static struct error *
directverify(struct ast_stmt *, struct state *);

struct error *
ast_stmt_verify(struct ast_stmt *stmt, struct state *s)
{
	struct state *copy = state_copy(s);
	struct error *err = directverify(stmt, copy);
	state_destroy(copy);
	return err;
}

static bool
islinearisable(struct ast_stmt *);

static struct error *
stmt_expr_verify(struct ast_stmt *, struct state *);

static struct error *
directverify(struct ast_stmt *stmt, struct state *s)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_NOP:
		return NULL;
	case STMT_EXPR:
		return stmt_expr_verify(stmt, s);
	default:
		return error_printf("verification blocks only support statement expressions");
	}
}

static bool
islinearisable(struct ast_stmt *stmt)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_DECLARATION: /* XXX: will have to be linearised with initialisation */
	case STMT_NOP:
	case STMT_LABELLED:
	case STMT_COMPOUND:
	case STMT_COMPOUND_V:
	case STMT_ITERATION_E:
	case STMT_ITERATION:
		return false;
	case STMT_SELECTION:
	case STMT_EXPR:
	case STMT_JUMP:
		return true;
	default:
		assert(false);
	}
}

static bool
islinearisable_setuponly(struct ast_stmt *stmt)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_DECLARATION: /* XXX: will have to be linearised with initialisation */
	case STMT_NOP:
	case STMT_LABELLED:
	case STMT_COMPOUND:
	case STMT_COMPOUND_V:
	case STMT_ITERATION_E:
	case STMT_ITERATION:
	case STMT_JUMP:
	case STMT_EXPR:
		return false;
	case STMT_SELECTION:
		return true;
	default:
		assert(false);
	}
}

/* stmt_expr_verify */

static struct error *
stmt_expr_verify(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *expr = ast_stmt_as_expr(stmt);
	if (!ast_expr_isverifiable(expr)) {
		return error_printf(
			"cannot verify complex expressions: `%s'",
			ast_expr_str(expr)
		);
	}
	struct bool_res *res = ast_expr_decide(expr, state);
	if (bool_res_iserror(res)) {
		return bool_res_as_error(res);
	}
	if (bool_res_as_bool(res)) {
		return NULL;
	}
	return error_printf("cannot verify statement");
}


/* stmt_exec */

static struct error *
stmt_decl_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_compoundv_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_compound_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_expr_exec(struct ast_expr *, struct state *);

static struct error *
stmt_sel_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_iter_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_jump_exec(struct ast_stmt *, struct state *);

static struct error *
stmt_register_exec(struct ast_stmt *, struct state *);

struct error *
ast_stmt_exec(struct ast_stmt *stmt, struct state *s)
{
	if (!state_islinear(s) && islinearisable(stmt)) {
		return linearise(stmt, s);
	}
	switch (ast_stmt_kind(stmt)) {
	case STMT_DECLARATION:
		return stmt_decl_exec(stmt, s);
	case STMT_NOP:
		return NULL;
	case STMT_LABELLED:
		a_printf(ast_stmt_ispre(stmt), "only setup labels supported\n");
		return NULL;
	case STMT_EXPR:
		return stmt_expr_exec(ast_stmt_as_expr(stmt), s);
	case STMT_COMPOUND:
		return stmt_compound_exec(stmt, s);
	case STMT_COMPOUND_V:
		return stmt_compoundv_exec(stmt, s);
	case STMT_SELECTION:
		return stmt_sel_exec(stmt, s);
	case STMT_ITERATION:
		return stmt_iter_exec(stmt, s);
	case STMT_JUMP:
		return stmt_jump_exec(stmt, s);
	case STMT_REGISTER:
		return stmt_register_exec(stmt, s);
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
		return stmt_expr_exec(assign, s);
	}
	return NULL;
}

static struct error *
stmt_compoundv_exec(struct ast_stmt *stmt, struct state *state)
{
	struct frame *block_frame = frame_blockverify_create(
		dynamic_str("verification block"), ast_stmt_as_block(stmt)
	);
	state_pushframe(state, block_frame);
	return NULL;
}

static struct error *
stmt_compound_exec(struct ast_stmt *stmt, struct state *state)
{
	struct frame *block_frame = frame_blocksame_create(
		dynamic_str("block"),
		ast_stmt_as_block(stmt),
		state
	);
	state_pushframe(state, block_frame);
	return NULL;
}

static struct error *
stmt_expr_exec(struct ast_expr *expr, struct state *state)
{
	struct e_res *res = ast_expr_eval(expr, state);
	if (e_res_iserror(res)) {
		return e_res_as_error(res);
	}
	e_res_destroy(res); 
	return NULL;
}

/* stmt_sel_exec */

static struct error *
stmt_sel_exec(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *cond = ast_stmt_sel_cond(stmt);
	struct ast_stmt *body = ast_stmt_sel_body(stmt),
			*nest = ast_stmt_sel_nest(stmt);

	struct bool_res *res = ast_expr_decide(cond, state);
	if (bool_res_iserror(res)) {
		return bool_res_as_error(res);
	}
	if (bool_res_as_bool(res)) {
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
stmt_iter_exec(struct ast_stmt *stmt, struct state *state)
{
	/* TODO: check internal consistency of iteration */

	struct ast_stmt *neteffect = iter_neteffect(stmt);
	if (!neteffect) {
		return NULL;
	}

	struct error *err = ast_stmt_exec(neteffect, state);
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
			struct error *err = e_res_as_error(res);
			if (error_to_eval_void(err)) {
				e_res_errorignore(res);
			} else {
				return err;
			}
		}
		if (e_res_haseval(res)) {
			struct eval *eval = e_res_as_eval(res);
			struct ast_type *spec_t = state_getreturntype(state),
					*rv_t = eval_type(eval);
			if (!ast_type_compatible(spec_t, rv_t)) {
				char *spec_t_str = ast_type_str(spec_t),
				     *rv_t_str = ast_type_str(rv_t);
				struct error *err = error_printf(
					"cannot return %s as %s",
					rv_t_str,
					spec_t_str
				);
				free(rv_t_str);
				free(spec_t_str);
				return err;
			}
			struct value *v = value_copy(
				value_res_as_value(eval_to_value(eval, state))
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
	struct e_res *res = ast_expr_eval(call, state);
	if (e_res_iserror(res)) {
		return e_res_as_error(res);
	}
	return NULL;
}

static struct e_res *
call_return(struct state *);

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

	struct e_res *r_res = call_return(state);
	if (e_res_iserror(r_res)) {
		struct error *err = e_res_as_error(r_res);
		if (error_to_eval_void(err)) {
			e_res_errorignore(r_res);
		} else {
			return err;
		}
	}
	if (e_res_haseval(r_res)) {
		object_assign(obj, eval_as_rval(e_res_as_eval(r_res)));
	}
	return NULL;
}

static struct e_res *
call_return(struct state *state)
{
	struct value *v = state_readregister(state);
	if (!v) {
		return e_res_empty_create();
	}
	state_popregister(state);
	return e_res_eval_create(
		eval_rval_create(calloralloc_type(state_framecall(state), state), v)
	);
}


static struct error *
labelled_pushsetup(struct ast_stmt *, struct state *);

static struct error *
sel_pushsetup(struct ast_stmt *stmt, struct state *);

static struct error *
comp_pushsetup(struct ast_stmt *stmt, struct state *);

struct error *
ast_stmt_pushsetup(struct ast_stmt *stmt, struct state *s)
{
	if (!state_islinear(s) && islinearisable_setuponly(stmt)) {
		return linearise(stmt, s);
	}
	switch (ast_stmt_kind(stmt)) {
	case STMT_DECLARATION:
		return stmt_decl_exec(stmt, s);
	case STMT_NOP:
	case STMT_EXPR:
	case STMT_JUMP:
	case STMT_ITERATION:
		return NULL;
	case STMT_LABELLED:
		return labelled_pushsetup(stmt, s);
	case STMT_SELECTION:
		return sel_pushsetup(stmt, s);
	case STMT_COMPOUND:
		return comp_pushsetup(stmt, s);
	case STMT_REGISTER:
		return stmt_register_exec(stmt, s);
	default:
		assert(false);
	}
}

static struct error *
labelled_pushsetup(struct ast_stmt *stmt, struct state *state)
{
	a_printf(ast_stmt_ispre(stmt), "only setup labels supported in abstract\n");

	struct ast_stmt *inner = ast_stmt_labelled_stmt(stmt);
	if (ast_stmt_kind(inner) == STMT_SELECTION) {
		return error_printf("setup preconditions must be decidable");
	}

	struct frame *setup_frame = frame_setup_create(
		dynamic_str("setup"),
		ast_stmt_labelled_as_block(stmt)
	);
	state_pushframe(state, setup_frame);
	return NULL;
}


static struct error *
sel_pushsetup(struct ast_stmt *stmt, struct state *state)
{
	struct ast_expr *cond = ast_stmt_sel_cond(stmt);
	struct ast_stmt *body = ast_stmt_sel_body(stmt),
			*nest = ast_stmt_sel_nest(stmt);
	struct bool_res *res = ast_expr_decide(cond, state);
	if (bool_res_iserror(res)) {
		return bool_res_as_error(res);
	}
	if (bool_res_as_bool(res)) {
		return ast_stmt_pushsetup(body, state);
	} else if (nest) {
		return ast_stmt_pushsetup(nest, state);
	}
	return NULL;
}

static struct error *
comp_pushsetup(struct ast_stmt *stmt, struct state *state)
{
	struct frame *block_frame = frame_blocksame_create(
		dynamic_str("block"),
		ast_stmt_as_block(stmt),
		state
	);
	state_pushframe(state, block_frame);
	return NULL;
}
