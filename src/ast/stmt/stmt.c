#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "expr/expr.h"
#include "intern.h"
#include "lex.h"
#include "props.h"
#include "state.h"
#include "stmt.h"
#include "util.h"
#include "value.h"

struct ast_stmt {
	enum ast_stmt_kind kind;
	union {
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
		struct {
			struct ast_stmt *init, *cond, *body;
			struct ast_expr *iter;
			struct ast_block *abstract;
		} iteration;
		struct ast_expr *expr;
		struct {
			enum ast_jump_kind kind;
			struct ast_expr *rv;
		} jump;
		struct {
			enum ast_alloc_kind kind;
			struct ast_expr *arg;
		} alloc;
		struct {
			bool iscall;
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

bool
ast_stmt_ispre(struct ast_stmt *stmt)
{
	return stmt->kind == STMT_LABELLED
		&& strcmp(stmt->u.labelled.label, "setup") == 0;
}

bool
ast_stmt_isassume(struct ast_stmt *stmt)
{
	return stmt->kind == STMT_LABELLED
		&& strcmp(stmt->u.labelled.label, "assume") == 0;
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
	if (ast_block_ndecls(b) == 0 && ast_block_nstmts(b) == 1) {
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

struct ast_stmt *
ast_stmt_create_jump(struct lexememarker *loc, enum ast_jump_kind kind,
		struct ast_expr *rv)
{
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_JUMP;
	stmt->u.jump.kind = JUMP_RETURN;
	stmt->u.jump.rv = rv;
	return stmt;
}

struct ast_expr *
ast_stmt_jump_rv(struct ast_stmt *stmt)
{
	return stmt->u.jump.rv;
}

static bool
sel_isterminal(struct ast_stmt *stmt, struct state *s);

bool
ast_stmt_isterminal(struct ast_stmt *stmt, struct state *s)
{
	switch (stmt->kind) {
	case STMT_JUMP:
		return stmt->u.jump.kind == JUMP_RETURN;
	case STMT_COMPOUND:
		return ast_block_isterminal(stmt->u.compound, s);
	case STMT_SELECTION:
		return sel_isterminal(stmt, s);
	default:
		return false;
	}
}

static bool
sel_isterminal(struct ast_stmt *stmt, struct state *s)
{
	struct decision dec = sel_decide(ast_stmt_sel_cond(stmt), s);
	assert(!dec.err);
	if (dec.decision) {
		return ast_stmt_isterminal(ast_stmt_sel_body(stmt), s);
	}
	return false;
}

bool
ast_stmt_isselection(struct ast_stmt *stmt)
{
	return stmt->kind == STMT_SELECTION;
}

static void
ast_stmt_destroy_jump(struct ast_stmt *stmt)
{
	struct ast_expr *rv = stmt->u.jump.rv;
	if (!rv) {
		return;
	}
	assert(stmt->u.jump.kind == JUMP_RETURN);
	ast_expr_destroy(rv);
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
ast_stmt_create_iter(struct lexememarker *loc,
		struct ast_stmt *init, struct ast_stmt *cond,
		struct ast_expr *iter, struct ast_block *abstract,
		struct ast_stmt *body)
{
	assert(init && cond && iter && abstract && body);
	struct ast_stmt *stmt = ast_stmt_create(loc);
	stmt->kind = STMT_ITERATION;
	stmt->u.iteration.init = init;
	stmt->u.iteration.cond = cond;
	stmt->u.iteration.iter = iter;
	stmt->u.iteration.body = body;
	stmt->u.iteration.abstract = abstract;
	return stmt;
}

struct ast_stmt *
ast_stmt_iter_init(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_ITERATION);
	return stmt->u.iteration.init;
}

struct ast_stmt *
ast_stmt_iter_cond(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_ITERATION);
	return stmt->u.iteration.cond;
}

struct ast_expr *
ast_stmt_iter_iter(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_ITERATION);
	return stmt->u.iteration.iter;
}

struct ast_block *
ast_stmt_iter_abstract(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_ITERATION);
	return stmt->u.iteration.abstract;
}

struct ast_stmt *
ast_stmt_iter_body(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_ITERATION);
	return stmt->u.iteration.body;
}

struct ast_expr *
ast_stmt_iter_lower_bound(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_ITERATION);
	struct ast_stmt *init = stmt->u.iteration.init;
	assert(init->kind == STMT_EXPR);
	return ast_expr_assignment_rval(init->u.expr);
}

struct ast_expr *
ast_stmt_iter_upper_bound(struct ast_stmt *stmt)
{
	assert(stmt->kind == STMT_ITERATION);
	struct ast_stmt *cond = stmt->u.iteration.cond;
	assert(cond->kind == STMT_EXPR);
	return ast_expr_binary_e2(cond->u.expr);
}

struct ast_stmt *
ast_stmt_register_call_create(struct ast_expr *call) {
	struct ast_stmt *stmt = ast_stmt_create(NULL);
	stmt->kind = STMT_REGISTER;
	stmt->u._register.iscall = true;
	stmt->u._register.op.call = call;
	return stmt;
}

struct ast_stmt *
ast_stmt_register_read_create(struct ast_variable *temp) {
	struct ast_stmt *stmt = ast_stmt_create(NULL);
	stmt->kind = STMT_REGISTER;
	stmt->u._register.iscall = false;
	stmt->u._register.op.temp = temp;
	return stmt;
}

static struct ast_stmt *
ast_stmt_copy_iter(struct ast_stmt *stmt)
{
	stmt->kind = STMT_ITERATION;
	struct ast_stmt *copy = ast_stmt_copy(stmt);
	stmt->kind = STMT_ITERATION_E;
	return ast_stmt_create_iter_e(copy);
}

static void
ast_stmt_iter_sprint(struct ast_stmt *stmt, int indent_level, struct strbuilder *b)
{
	assert(stmt->kind == STMT_ITERATION);
	char *init = ast_stmt_str(stmt->u.iteration.init, indent_level),
	     *cond = ast_stmt_str(stmt->u.iteration.cond, indent_level),
	     *body = ast_stmt_str(stmt->u.iteration.body, indent_level),
	     *iter = ast_expr_str(stmt->u.iteration.iter);

	char *abs = stmt->u.iteration.abstract ?
		ast_block_absstr(stmt->u.iteration.abstract, indent_level) : "";

	strbuilder_printf(
		b,
		"for (%s %s %s) ~ %s%s",
		init, cond, iter, abs, body
	);

	free(init); free(cond); free(body); free(iter);
}

struct ast_stmt *
ast_stmt_create_iter_e(struct ast_stmt *stmt)
{
	/* TODO: determine where loc should go */
	assert(stmt->kind == STMT_ITERATION);
	stmt->kind = STMT_ITERATION_E;
	return stmt;
}

static void
ast_stmt_iter_e_sprint(struct ast_stmt *stmt, int indent_level, struct strbuilder *b)
{
	assert(stmt->kind == STMT_ITERATION_E);
	stmt->kind = STMT_ITERATION;
	char *s = ast_stmt_str(stmt, indent_level);
	stmt->kind = STMT_ITERATION_E;
	strbuilder_printf(b, ".%s", s);
	free(s);
}

static void
ast_stmt_jump_sprint(struct ast_stmt *stmt, struct strbuilder *b)
{
	assert(stmt->kind == STMT_JUMP);
	char *rv = ast_expr_str(stmt->u.jump.rv);

	strbuilder_printf(
		b,
		"return %s;",
		rv
	);

	free(rv);
}

static void
ast_stmt_register_sprint(struct ast_stmt *stmt, struct strbuilder *b)
{
	assert(stmt->kind == STMT_REGISTER);
	if (stmt->u._register.iscall) {
		char *call = ast_expr_str(stmt->u._register.op.call);
		strbuilder_printf(b, "call %s;", call);
	} else {
		char *tempvar = ast_variable_str(stmt->u._register.op.temp);
		strbuilder_printf(b, "mov %s;", tempvar);
	}
}

static struct ast_expr *
ast_expr_copy_ifnotnull(struct ast_expr *expr)
{
	return expr ? ast_expr_copy(expr) : NULL;
}

void
ast_stmt_destroy(struct ast_stmt *stmt)
{
	switch (stmt->kind) {
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
	case STMT_ITERATION_E:
		ast_stmt_destroy(stmt->u.iteration.init);
		ast_stmt_destroy(stmt->u.iteration.cond);
		ast_stmt_destroy(stmt->u.iteration.body);
		ast_expr_destroy(stmt->u.iteration.iter);
		ast_block_destroy(stmt->u.iteration.abstract);
		break;
	case STMT_EXPR:
		ast_expr_destroy(stmt->u.expr);
		break;
	case STMT_JUMP:
		ast_stmt_destroy_jump(stmt);
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
		return ast_stmt_create_iter(
			loc,
			ast_stmt_copy(stmt->u.iteration.init),
			ast_stmt_copy(stmt->u.iteration.cond),
			ast_expr_copy(stmt->u.iteration.iter),
			ast_block_copy(stmt->u.iteration.abstract),
			ast_stmt_copy(stmt->u.iteration.body)
		);
	case STMT_ITERATION_E:
		return ast_stmt_copy_iter(stmt);
	case STMT_JUMP:
		return ast_stmt_create_jump(
			loc, stmt->u.jump.kind,
			ast_expr_copy_ifnotnull(stmt->u.jump.rv)
		);
	case STMT_REGISTER:
		return stmt->u._register.iscall
			? ast_stmt_register_call_create(stmt->u._register.op.call)
			: ast_stmt_register_read_create(stmt->u._register.op.temp);
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
	case STMT_ITERATION_E:
		ast_stmt_iter_e_sprint(stmt, indent_level, b);
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
ast_stmt_iteration_getfuncs(struct ast_stmt *stmt);

static struct string_arr *
ast_stmt_compound_getfuncs(struct ast_stmt *stmt);

struct string_arr *
ast_stmt_getfuncs(struct ast_stmt *stmt)
{
	switch (stmt->kind) {
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
	case STMT_ITERATION_E:
		return ast_stmt_iteration_getfuncs(stmt);
	case STMT_JUMP:
		return ast_expr_getfuncs(stmt->u.jump.rv);
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
ast_stmt_iteration_getfuncs(struct ast_stmt *stmt)
{
	struct ast_stmt *init = stmt->u.iteration.init,
			*cond = stmt->u.iteration.cond,
			*body = stmt->u.iteration.body;
	struct ast_expr *iter = stmt->u.iteration.iter;
	/* XXX: inlucde loop abstracts potentially */
	struct string_arr *init_arr = ast_stmt_getfuncs(init),
			  *cond_arr = ast_stmt_getfuncs(cond),
			  *body_arr = ast_stmt_getfuncs(body),
			  *iter_arr = ast_expr_getfuncs(iter);
	
	return string_arr_concat(
		string_arr_create(),
		string_arr_concat(
			string_arr_concat(init_arr, cond_arr),
			string_arr_concat(body_arr, iter_arr)
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

#include "verify.c"
