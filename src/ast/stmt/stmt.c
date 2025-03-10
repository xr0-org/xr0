#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "expr.h"
#include "intern.h"
#include "lex.h"
#include "state.h"
#include "util.h"
#include "value.h"

#include "iter.h"
#include "jump.h"
#include "stmt.h"

struct ast_stmt {
	enum ast_stmt_kind kind;
	union {
		struct {
			struct ast_variable_arr *vars;
		} declaration;
		struct {
			char *label;
			struct ast_stmt *stmt;
		} labelled;
		struct ast_block *compound;
		struct {
			bool isswitch;
			struct ast_expr *cond;
			struct ast_stmt *body;
			struct ast_stmt *nest;
		} selection;
		struct iter *iter;
		struct ast_expr *expr;
		struct jump *jump;
		struct {
			enum ast_alloc_kind kind;
			struct ast_expr *arg;
		} alloc;
		struct {
			enum ast_register_kind {
				REGISTER_SETUPV,
				REGISTER_CALL,
				REGISTER_MOV,
			} kind;
			union {
				struct ast_expr *call;
				struct ast_variable *temp;
			} op;
		} _register;
	} u;

	struct lexememarker *loc;
};

struct lexememarker *
ast_stmt_lexememarker(struct ast_stmt *stmt)
{
	return stmt->loc;
}

static struct ast_stmt *
ast_stmt_create(struct lexememarker *loc)
{
	struct ast_stmt *stmt = calloc(1, sizeof(struct ast_stmt));
	stmt->loc = loc;
	return stmt;
}

struct ast_stmt *
ast_stmt_create_declaration(struct lexememarker *loc, struct ast_variable_arr *vars)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_DECLARATION;
	stmt->u.declaration.vars = vars;
	return stmt;
}

struct ast_variable_arr *
ast_stmt_declaration_vars(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_DECLARATION);
	return stmt->u.declaration.vars;
}

static void
ast_stmt_declaration_sprint(struct ast_stmt *stmt, struct strbuilder *b)
{
	struct ast_variable_arr *vars = stmt->u.declaration.vars;
	for (int i = 0; i < ast_variable_arr_n(vars); i++) {
		struct ast_variable **v = ast_variable_arr_v(vars);
		char *s = ast_variable_str(v[i]);
		strbuilder_printf(b, "%s;", s);
		free(s);
	}
}

bool
ast_stmt_isdecl(struct ast_stmt *stmt)
{
	return stmt->kind == STMT_DECLARATION;
}

struct ast_stmt *
ast_stmt_create_labelled(struct lexememarker *loc, char *label,
		struct ast_stmt *substmt)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_LABELLED;
	stmt->u.labelled.label = label;
	stmt->u.labelled.stmt = substmt;
	return stmt;
}

char *
ast_stmt_labelled_label(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_LABELLED);

	return stmt->u.labelled.label;
}

struct ast_stmt *
ast_stmt_labelled_stmt(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_LABELLED);

	return stmt->u.labelled.stmt;
}

static struct ast_block *
ast_stmt_to_block(struct ast_stmt *);

struct ast_block *
ast_stmt_labelled_as_block(struct ast_stmt *stmt)
{
	assert(ast_stmt_issetup(stmt));
	struct ast_stmt *setup = ast_stmt_labelled_stmt(stmt);
	switch (setup->kind) {
	case STMT_EXPR:
		return ast_stmt_to_block(ast_stmt_copy(setup));
	case STMT_COMPOUND:
		return ast_block_copy(ast_stmt_as_block(setup));
	default:
		assert(false);
	}
}

static struct ast_block *
ast_stmt_to_block(struct ast_stmt *stmt)
{
	struct ast_block *b = ast_block_create(NULL, 0);
	ast_block_append_stmt(b, stmt);
	return b;
}

struct ast_stmt *
ast_stmt_as_compound(struct ast_stmt *stmt)
{
	if (stmt->kind == STMT_COMPOUND) {
		return stmt;
	}
	struct ast_block *b = ast_block_create(NULL, 0);
	ast_block_append_stmt(b, stmt);
	return ast_stmt_create_compound(
		lexememarker_copy(ast_stmt_lexememarker(stmt)), b
	);
}

bool
ast_stmt_issetup(struct ast_stmt *stmt)
{
	return stmt->kind == STMT_LABELLED
		&& strcmp(stmt->u.labelled.label, "setup") == 0;
}

static void
ast_stmt_labelled_sprint(struct ast_stmt *stmt, int indent_level,
		struct strbuilder *b)
{
	char *s = ast_stmt_str(stmt->u.labelled.stmt, indent_level);
	strbuilder_printf(b, "%s: %s", stmt->u.labelled.label, s);
	free(s);
}

struct ast_stmt *
ast_stmt_create_nop(struct lexememarker *loc)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_NOP;
	return stmt;
}

static void
ast_stmt_nop_sprint(struct ast_stmt *stmt, struct strbuilder *b)
{
	strbuilder_printf(b, ";");
}

struct ast_stmt *
ast_stmt_create_expr(struct lexememarker *loc, struct ast_expr *expr)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_EXPR;
	stmt->u.expr = expr;
	return stmt;
}

static void
ast_stmt_expr_sprint(struct ast_stmt *stmt, struct strbuilder *b)
{
	assert(stmt->kind == STMT_EXPR);
	char *s = ast_expr_str(stmt->u.expr);
	strbuilder_printf(b, "%s;", s);
	free(s);
}

struct ast_stmt *
ast_stmt_create_compound(struct lexememarker *loc, struct ast_block *b)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_COMPOUND;
	stmt->u.compound = b;
	return stmt;
}

struct ast_block *
ast_stmt_as_block(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_COMPOUND || stmt->kind == STMT_COMPOUND_V);
	return stmt->u.compound;
}

static void
ast_stmt_compound_sprint(struct ast_stmt *stmt, int indent_level,
		struct strbuilder *b)
{
	assert(stmt->kind == STMT_COMPOUND);
	char *s = ast_block_str(stmt->u.compound, indent_level);
	strbuilder_printf(b, s);
	free(s);
}

static void
ast_stmt_compound_v_sprint(struct ast_stmt *stmt, int indent_level,
		struct strbuilder *sb)
{
	struct ast_block *b = ast_stmt_as_block(stmt);

	/* special case for nice print */
	if (ast_block_nstmts(b) == 1) {
		char *s = ast_stmt_str(ast_block_stmts(b)[0], 0);
		strbuilder_printf(sb, "~ [ %s ]", s);
		free(s);
	} else {
		char *s = ast_block_absstr(stmt->u.compound, indent_level);
		strbuilder_printf(sb, "~ %s", s);
		free(s);
	}
}

struct ast_stmt *
ast_stmt_create_compound_v(struct lexememarker *loc, struct ast_block *b)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_COMPOUND_V;
	stmt->u.compound = b;
	return stmt;
}

static struct ast_stmt *
ast_stmt_create_jump(struct lexememarker *loc, struct jump *j)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_JUMP;
	stmt->u.jump = j;
	return stmt;
}

struct ast_stmt *
ast_stmt_create_break(struct lexememarker *loc)
{
	return ast_stmt_create_jump(loc, jump_break_create());
}

struct ast_stmt *
ast_stmt_create_return(struct lexememarker *loc, struct ast_expr *rv)
{
	return ast_stmt_create_jump(loc, jump_return_create(rv));
}

int
ast_stmt_isjump(struct ast_stmt *stmt)
{
	return stmt->kind == STMT_JUMP;
}

struct jump *
ast_stmt_as_jump(struct ast_stmt *stmt)
{
	assert(ast_stmt_isjump(stmt));
	return stmt->u.jump;
}

int
ast_stmt_isreturn(struct ast_stmt *stmt)
{
	return jump_isreturn(ast_stmt_as_jump(stmt));
}

int
ast_stmt_isbreak(struct ast_stmt *stmt)
{
	return jump_isbreak(ast_stmt_as_jump(stmt));
}

struct ast_stmt *
ast_stmt_create_sel(struct lexememarker *loc, bool isswitch, struct ast_expr *cond,
		struct ast_stmt *body, struct ast_stmt *nest)
{
	assert(!isswitch); /* XXX */
	assert(cond);
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_SELECTION;
	stmt->u.selection.isswitch = isswitch;
	stmt->u.selection.cond = cond;
	stmt->u.selection.body = body;
	stmt->u.selection.nest = nest;
	return stmt;
}

struct ast_expr *
ast_stmt_sel_cond(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_SELECTION);
	return stmt->u.selection.cond;
}

struct ast_stmt *
ast_stmt_sel_body(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_SELECTION);
	return stmt->u.selection.body;
}

struct ast_stmt *
ast_stmt_sel_nest(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_SELECTION);
	return stmt->u.selection.nest;
}

static void
ast_stmt_sel_sprint(struct ast_stmt *stmt, int indent_level, struct strbuilder *b)
{
	assert(stmt->kind == STMT_SELECTION);
	char *cond	= ast_expr_str(stmt->u.selection.cond),
	     *body	= ast_stmt_str(stmt->u.selection.body, indent_level);

	strbuilder_printf(
		b,
		"if (%s) %s",
		cond, body
	);

	struct ast_stmt *nest_stmt = stmt->u.selection.nest;
	if (nest_stmt) {
		char *nest = ast_stmt_str(nest_stmt, indent_level);
		strbuilder_printf(
			b,
			" else %s",
			nest
		);
		free(nest);
	}

	free(cond); free(body);
}

struct ast_stmt *
ast_stmt_create_iter(struct lexememarker *loc, struct iter *iter)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_ITERATION;
	stmt->u.iter = iter;
	return stmt;
}

struct ast_stmt *
ast_stmt_create_for(struct lexememarker *loc, struct ast_stmt *init,
		struct ast_stmt *cond, struct ast_expr *update,
		struct ast_block *inv, struct ast_stmt *body)
{
	return ast_stmt_create_iter(
		loc, iter_for_create(init, cond, update, inv, body)
	);
}

struct iter *
ast_stmt_as_iter(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_ITERATION);
	return stmt->u.iter;
}

struct ast_stmt *
ast_stmt_register_setupv_create(struct lexememarker *loc, struct ast_expr *call)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_REGISTER;
	stmt->u._register.kind = REGISTER_SETUPV;
	stmt->u._register.op.call = call;
	return stmt;
}

struct ast_stmt *
ast_stmt_register_call_create(struct lexememarker *loc, struct ast_expr *call)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_REGISTER;
	stmt->u._register.kind = REGISTER_CALL;
	stmt->u._register.op.call = call;
	return stmt;
}

struct ast_stmt *
ast_stmt_register_mov_create(struct lexememarker *loc, struct ast_variable *temp)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_REGISTER;
	stmt->u._register.kind = REGISTER_MOV;
	stmt->u._register.op.temp = temp;
	return stmt;
}

bool
ast_stmt_register_issetupv(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_REGISTER);
	return stmt->u._register.kind == REGISTER_SETUPV;
}

bool
ast_stmt_register_iscall(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_REGISTER);
	return stmt->u._register.kind == REGISTER_CALL;
}

struct ast_expr *
ast_stmt_register_call(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_REGISTER);
	return stmt->u._register.op.call;
}

struct ast_variable *
ast_stmt_register_mov(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_REGISTER);
	return stmt->u._register.op.temp;
}

static void
ast_stmt_iter_sprint(struct ast_stmt *stmt, int indent_level,
		struct strbuilder *b)
{
	assert(stmt->kind == STMT_ITERATION);
	iter_sprint(stmt->u.iter, indent_level, b);
}

static void
ast_stmt_jump_sprint(struct ast_stmt *stmt, struct strbuilder *b)
{
	char *s = jump_str(stmt->u.jump);
	strbuilder_printf(b, "%s\n", s);
	free(s);
}

static void
ast_stmt_register_sprint(struct ast_stmt *stmt, struct strbuilder *b)
{
	assert(stmt->kind == STMT_REGISTER);
	char *call;
	switch (stmt->u._register.kind) {
	case REGISTER_SETUPV:
		call = ast_expr_str(stmt->u._register.op.call);
		strbuilder_printf(b, "setupv %s;", call);
		free(call);
		break;
	case REGISTER_CALL:
		call = ast_expr_str(stmt->u._register.op.call);
		strbuilder_printf(b, "call %s;", call);
		free(call);
		break;
	case REGISTER_MOV:
		strbuilder_printf(
			b,
			"movret %s;",
			ast_variable_name(stmt->u._register.op.temp)
		);
		break;
	default:
		assert(false);
	}
}

void
ast_stmt_destroy(struct ast_stmt *stmt)
{
	switch (stmt->kind) {
	case STMT_DECLARATION:
		for (int i = 0; i < ast_variable_arr_n(stmt->u.declaration.vars); i++) {
			ast_variable_destroy(
				ast_variable_arr_v(stmt->u.declaration.vars)[i]
			);
		}
		break;
	case STMT_LABELLED:
		free(stmt->u.labelled.label);
		ast_stmt_destroy(stmt->u.labelled.stmt);
		break;
	case STMT_NOP:
		break;
	case STMT_COMPOUND:
	case STMT_COMPOUND_V:
		ast_block_destroy(stmt->u.compound);
		break;
	case STMT_SELECTION:
		ast_expr_destroy(stmt->u.selection.cond);
		ast_stmt_destroy(stmt->u.selection.body);
		if (stmt->u.selection.nest) {
			ast_stmt_destroy(stmt->u.selection.nest);
		}
		break;
	case STMT_ITERATION:
		iter_destroy(stmt->u.iter);
		break;
	case STMT_EXPR:
		ast_expr_destroy(stmt->u.expr);
		break;
	case STMT_JUMP:
		jump_destroy(stmt->u.jump);
		break;
	default:
		assert(false);
		break;
	}
	if (stmt->loc) {
		lexememarker_destroy(stmt->loc);
	}
	free(stmt);
}

struct ast_stmt *
ast_stmt_copy(struct ast_stmt *stmt)
{
	struct lexememarker *loc = stmt->loc
		? lexememarker_copy(stmt->loc)
		: NULL;
	switch (stmt->kind) {
	case STMT_DECLARATION:
		return ast_stmt_create_declaration(
			loc,
			ast_variable_arr_copy(stmt->u.declaration.vars)
		);
	case STMT_LABELLED:
		return ast_stmt_create_labelled(
			loc,
			dynamic_str(stmt->u.labelled.label),
			ast_stmt_copy(stmt->u.labelled.stmt)
		);
	case STMT_NOP:
		return ast_stmt_create_nop(loc);
	case STMT_EXPR:
		return ast_stmt_create_expr(loc, ast_expr_copy(stmt->u.expr));
	case STMT_COMPOUND:
		return ast_stmt_create_compound(
			loc, ast_block_copy(stmt->u.compound)
		);
	case STMT_COMPOUND_V:
		return ast_stmt_create_compound_v(
			loc, ast_block_copy(stmt->u.compound)
		);
	case STMT_SELECTION:
		return ast_stmt_create_sel(
			loc,
			stmt->u.selection.isswitch,
			ast_expr_copy(stmt->u.selection.cond),
			ast_stmt_copy(stmt->u.selection.body),
			stmt->u.selection.nest
				? ast_stmt_copy(stmt->u.selection.nest)
				: NULL
		);
	case STMT_ITERATION:
		return ast_stmt_create_iter(loc, iter_copy(stmt->u.iter));
	case STMT_JUMP:
		return ast_stmt_create_jump(loc, jump_copy(stmt->u.jump));
	case STMT_REGISTER:
		switch (stmt->u._register.kind) {
		case REGISTER_SETUPV:
			return ast_stmt_register_setupv_create(
				loc, stmt->u._register.op.call
			);
		case REGISTER_CALL:
			return ast_stmt_register_call_create(
				loc, stmt->u._register.op.call
			);
		case REGISTER_MOV:
			return ast_stmt_register_mov_create(
				loc, stmt->u._register.op.temp
			);
		default:
			assert(false);
		}
	default:
		assert(false);
	}
}

char *
ast_stmt_str(struct ast_stmt *stmt, int indent_level)
{
	assert(stmt);
	struct strbuilder *b = strbuilder_create();
	switch (stmt->kind) {
	case STMT_DECLARATION:
		ast_stmt_declaration_sprint(stmt, b);
		break;
	case STMT_LABELLED:
		ast_stmt_labelled_sprint(stmt, indent_level, b);
		break;
	case STMT_NOP:
		ast_stmt_nop_sprint(stmt, b);
		break;
	case STMT_EXPR:
		ast_stmt_expr_sprint(stmt, b);
		break;
	case STMT_COMPOUND:
		ast_stmt_compound_sprint(stmt, indent_level, b);
		break;
	case STMT_COMPOUND_V:
		ast_stmt_compound_v_sprint(stmt, indent_level, b);
		break;
	case STMT_SELECTION:
		ast_stmt_sel_sprint(stmt, indent_level, b);
		break;
	case STMT_ITERATION:
		ast_stmt_iter_sprint(stmt, indent_level, b);
		break;
	case STMT_JUMP:
		ast_stmt_jump_sprint(stmt, b);
		break;
	case STMT_REGISTER:
		ast_stmt_register_sprint(stmt, b);
		break;
	default:
		assert(false);
	}
	return strbuilder_build(b);
}

bool
ast_stmt_equal(struct ast_stmt *s1, struct ast_stmt *s2)
{
	if (!s1 || !s2) {
		return false;
	}
	if (ast_stmt_kind(s1) != ast_stmt_kind(s2)) {
		return false;
	}
	switch (ast_stmt_kind(s1)) {
	case STMT_EXPR:
		return ast_expr_equal(ast_stmt_as_expr(s1), ast_stmt_as_expr(s2));
	default:
		assert(false);
	}
}

enum ast_stmt_kind
ast_stmt_kind(struct ast_stmt *stmt)
{
	return stmt->kind;
}

struct ast_block *
ast_stmt_as_v_block(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_COMPOUND_V);
	return stmt->u.compound;
}

struct ast_expr *
ast_stmt_as_expr(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_EXPR);
	return stmt->u.expr;
}

static struct string_arr *
ast_stmt_expr_getfuncs(struct ast_stmt *stmt);

static struct string_arr *
ast_stmt_selection_getfuncs(struct ast_stmt *stmt);

static struct string_arr *
ast_stmt_compound_getfuncs(struct ast_stmt *stmt);

struct string_arr *
ast_stmt_getfuncs(struct ast_stmt *stmt)
{
	switch (stmt->kind) {
	case STMT_DECLARATION: /* XXX: update when adding initalisation */
	case STMT_NOP:
		return string_arr_create();
	case STMT_LABELLED:
		return ast_stmt_getfuncs(stmt->u.labelled.stmt);
	case STMT_COMPOUND:
	case STMT_COMPOUND_V:
		return ast_stmt_compound_getfuncs(stmt);
	case STMT_EXPR:
		return ast_stmt_expr_getfuncs(stmt);
	case STMT_SELECTION:
		return ast_stmt_selection_getfuncs(stmt);
	case STMT_ITERATION:
		return iter_getfuncs(stmt->u.iter);
	case STMT_JUMP:
		return jump_getfuncs(stmt->u.jump);
	default:
		assert(false);
	}
}

static struct string_arr *
ast_stmt_expr_getfuncs(struct ast_stmt *stmt)
{
	return ast_expr_getfuncs(stmt->u.expr);
}

static struct string_arr *
ast_stmt_selection_getfuncs(struct ast_stmt *stmt)
{
	struct ast_expr *cond = stmt->u.selection.cond;
	struct ast_stmt *body = stmt->u.selection.body,
			*nest = stmt->u.selection.nest;
	struct string_arr *cond_arr = ast_expr_getfuncs(cond),
			  *body_arr = ast_stmt_getfuncs(body),
			  *nest_arr = nest ? ast_stmt_getfuncs(nest) : string_arr_create();
	
	return string_arr_concat(
		string_arr_create(),
		string_arr_concat(
			cond_arr,
			string_arr_concat(
				body_arr, nest_arr
			)
		)
	);
}

static struct string_arr *
ast_stmt_compound_getfuncs(struct ast_stmt *stmt)
{
	struct string_arr *res = string_arr_create();
	struct ast_block *b = stmt->u.compound;
	struct ast_stmt **stmts = ast_block_stmts(b);
	for (int i = 0; i < ast_block_nstmts(b); i++) {
		res = string_arr_concat(
			res,
			ast_stmt_getfuncs(stmts[i])
		);
		/* XXX: leaks */
	}
	return res;
}

static struct error *
preconds_selection_verify(struct ast_stmt *stmt);

static struct error *
preconds_compound_verify(struct ast_stmt *);

struct error *
ast_stmt_preconds_validate(struct ast_stmt *stmt)
{
	switch (stmt->kind) {
	case STMT_EXPR:
	case STMT_ALLOCATION:
	case STMT_ITERATION:
		return NULL;
	case STMT_SELECTION:
		return preconds_selection_verify(stmt);	
	case STMT_COMPOUND:
		return preconds_compound_verify(stmt);
	default:
		assert(false);
	}
}

static struct error *
preconds_selection_verify(struct ast_stmt *stmt)
{
	struct lexememarker *l = ast_stmt_lexememarker(stmt);
	return error_printf(
		"%s setup preconditions must be decidable", lexememarker_str(l)
	);
}

static struct error *
preconds_compound_verify(struct ast_stmt *stmt)
{
	struct error *err;
	struct ast_block *b = stmt->u.compound;
	struct ast_stmt **stmts = ast_block_stmts(b);
	for (int i = 0; i < ast_block_nstmts(b); i++) {
		if ((err = ast_stmt_preconds_validate(stmts[i]))) {
			return err;
		}
	}
	return NULL;
}

DEFINE_RESULT_TYPE(struct ast_stmt *, stmt, ast_stmt_destroy, ast_stmt_res, false)

static struct ast_stmt_res *
sel_setupdecide(struct ast_stmt *, struct state *);

static struct ast_stmt_res *
comp_setupdecide(struct ast_stmt *, struct state *);

struct ast_stmt_res *
ast_stmt_setupdecide(struct ast_stmt *stmt, struct state *s)
{
	switch (ast_stmt_kind(stmt)) {
	case STMT_EXPR:
	case STMT_ITERATION:
	case STMT_JUMP:
		return ast_stmt_res_error_create(error_modulate_skip());
	case STMT_DECLARATION:
	case STMT_NOP:
		/* TODO: */
		return ast_stmt_res_stmt_create(stmt);
	case STMT_LABELLED:
		a_printf(
			ast_stmt_issetup(stmt),
			"only setup labels supported in abstract\n"
		);
		return ast_stmt_res_stmt_create(stmt);
	case STMT_SELECTION:
		return sel_setupdecide(stmt, s);
	case STMT_COMPOUND:
		return comp_setupdecide(stmt, s);
	default:
		assert(false);
	}
}

static struct ast_stmt_res *
sel_setupdecide(struct ast_stmt *stmt, struct state *s)
{
	struct ast_expr *cond = ast_stmt_sel_cond(stmt);
	struct ast_stmt *body = ast_stmt_sel_body(stmt),
			*nest = ast_stmt_sel_nest(stmt);
	struct bool_res *res = ast_expr_decide(cond, s);
	if (bool_res_iserror(res)) {
		return ast_stmt_res_error_create(bool_res_as_error(res));
	}
	if (bool_res_as_bool(res)) {
		return ast_stmt_setupdecide(body, s);
	} else if (nest) {
		return ast_stmt_setupdecide(nest, s);
	}
	return ast_stmt_res_error_create(error_modulate_skip());
}

static struct ast_stmt_res *
comp_setupdecide(struct ast_stmt *stmt, struct state *s)
{
	struct ast_block_res *res = ast_block_setupdecide(
		ast_stmt_as_block(stmt), s
	);
	if (ast_block_res_iserror(res)) {
		return ast_stmt_res_error_create(ast_block_res_as_error(res));
	}
	struct ast_block *b = ast_block_res_as_block(res); 
	if (ast_block_nstmts(b) == 0) {
		return ast_stmt_res_error_create(error_modulate_skip());
	}
	return ast_stmt_res_stmt_create(
		ast_stmt_create_compound(
			lexememarker_copy(ast_stmt_lexememarker(stmt)), b
		)
	);
}
