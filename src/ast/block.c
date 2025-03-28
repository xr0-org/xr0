#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "util.h"

#include "expr.h"
#include "stmt.h"

struct ast_block {
	int nstmt;
	struct ast_stmt **stmt;
	int tempcount;
};

struct ast_block *
ast_block_create(struct ast_stmt **stmt, int nstmt)
{
	assert(nstmt > 0 || !stmt);

	struct ast_block *b = malloc(sizeof(struct ast_block));
	b->stmt = stmt;
	b->nstmt = nstmt;
	b->tempcount = 0;
	return b;
}

void
ast_block_destroy(struct ast_block *b)
{
	for (int i = 0; i < b->nstmt; i++) {
		ast_stmt_destroy(b->stmt[i]);
	}
	free(b->stmt);
	free(b);
}

static struct ast_stmt **
copy_stmt_arr(int len, struct ast_stmt **);

struct ast_block *
ast_block_copy(struct ast_block *b)
{
	assert(b);
	return ast_block_create(
		copy_stmt_arr(b->nstmt, b->stmt),
		b->nstmt
	);
}

static struct ast_stmt **
copy_stmt_arr(int len, struct ast_stmt **stmt)
{
	assert(len == 0 || stmt);
	if (len == 0) {
		return NULL;
	}
	struct ast_stmt **new = malloc(sizeof(struct ast_stmt *) * len); 
	for (int i = 0; i < len; i++) {
		new[i] = ast_stmt_copy(stmt[i]);
	}
	return new;
}

static char *
_indentation(int level);

static char *
ast_block_str_div(struct ast_block *b, int indent_level, char divst, char divend)
{
	assert(indent_level > 0);

	char *indent = _indentation(indent_level),
	     *previndent = _indentation(indent_level-1);

	struct strbuilder *sb = strbuilder_create();
	strbuilder_printf(sb, "%c\n", divst);
	for (int i = 0; i < b->nstmt; i++) {
		char *s = ast_stmt_str(b->stmt[i], indent_level+1);
		strbuilder_printf(sb, "%s%s\n", indent, s);
		free(s);
	}
	strbuilder_printf(sb, "%s%c", previndent, divend);

	free(previndent);
	free(indent);

	return strbuilder_build(sb);
}

#define INDENT_CHAR '\t'

static char *
_indentation(int level)
{
	assert(level >= 0);
	char *s = malloc(sizeof(char) * level + 1);
	for (int i = 0; i < level; i++) {
		s[i] = INDENT_CHAR;
	}
	s[level] = '\0';
	return s;
}




char *
ast_block_str(struct ast_block *b, int indent)
{
	return ast_block_str_div(b, indent, '{', '}');
}

char *
ast_block_absstr(struct ast_block *b, int indent)
{
	return ast_block_str_div(b, indent, '[', ']');
}

char *
ast_block_render(struct ast_block *b, int index)
{
	struct strbuilder *sb = strbuilder_create();
	for (int i = 0; i < b->nstmt; i++) {
		char *s = ast_stmt_str(b->stmt[i], 2);
		if (i == index) {
			strbuilder_printf(sb, "-->\t%s\n", s);
		} else {
			strbuilder_printf(sb, "\t%s\n", s);
		}
		free(s);
	}
	return strbuilder_build(sb);
}

int
ast_block_nstmts(struct ast_block *b)
{
	return b->nstmt;
}

struct ast_stmt **
ast_block_stmts(struct ast_block *b)
{
	assert(b->nstmt > 0 || !b->stmt);
	return b->stmt;
}

bool
ast_block_empty(struct ast_block *b)
{
	return b->nstmt == 0;
}

static int
ast_block_insert(struct ast_block *b, int index, struct ast_stmt *stmt)
{
	b->stmt = realloc(b->stmt, sizeof(struct ast_stmt *) * ++b->nstmt);
	assert(b->stmt);
	for (int i = b->nstmt-1; i > index; i--) {
		b->stmt[i] = b->stmt[i-1];
	}
	b->stmt[index] = stmt;
	return index;
}

void
ast_block_prepend_stmt(struct ast_block *b, struct ast_stmt *stmt)
{
	ast_block_insert(b, 0, stmt);
}

void
ast_block_append_stmt(struct ast_block *b, struct ast_stmt *stmt)
{
	ast_block_insert(b, b->nstmt, stmt);
}

void
ast_block_appendallcopy(struct ast_block *b, struct ast_block *from)
{
	for (int i = 0; i < from->nstmt; i++) {
		ast_block_append_stmt(b, ast_stmt_copy(from->stmt[i]));
	}
}

bool
ast_block_hastoplevelreturn(struct ast_block *b)
{
	for (int i = 0; i < b->nstmt; i++) {
		struct ast_stmt *stmt = b->stmt[i];
		if (ast_stmt_isjump(stmt) && ast_stmt_isreturn(stmt)) {
			return true;
		}
	}
	return false;
}

static char *
generate_tempvar(int tempid);

struct ast_expr *
ast_block_call_geninstr(struct ast_block *b, struct lexememarker *loc,
		struct ast_type *rtype, struct ast_expr *expr)
{
	ast_block_append_stmt(
		b,
		ast_stmt_asm_setupv_create(loc, ast_expr_copy(expr))
	);

	ast_block_append_stmt(
		b,
		ast_stmt_asm_call_create(loc, ast_expr_copy(expr))
	);

	if (!ast_type_isvoid(rtype)) {
		char *tvar = generate_tempvar(b->tempcount++);
		ast_block_append_stmt(
			b, 
			ast_stmt_asm_movret_create(
				loc,
				ast_variable_create(
					dynamic_str(tvar), ast_type_copy(rtype)
				)
			)
		);
		return ast_expr_identifier_create(tvar);
	}
	return NULL;
}

static char *
generate_tempvar(int tempid)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "#R%d", tempid);
	return strbuilder_build(b);
}

struct ast_expr *
ast_block_relop_geninstr(struct ast_block *b, struct lexememarker *loc,
		struct ast_expr *e, struct state *s)
{
	struct ast_expr *e1 = ast_expr_geninstr(ast_expr_binary_e1(e), loc, b, s),
			*e2 = ast_expr_geninstr(ast_expr_binary_e2(e), loc, b, s);
	char *tvar = generate_tempvar(b->tempcount++);
	ast_block_append_stmt(
		b, 
		ast_stmt_asm_mov_create(
			loc,
			ast_variable_create(
				dynamic_str(tvar),
				/* 3.3.8: The result has type int. */
				ast_type_create_int()
			),
			ast_expr_binary_create(e1, ast_expr_binary_op(e), e2)
		)
	);
	return ast_expr_identifier_create(tvar);
}

static struct ast_expr *
ast_block_l_and_geninstr(struct ast_block *b, struct lexememarker *loc,
		struct ast_expr *e1, struct ast_expr *e2, struct state *);

static struct ast_expr *
ast_block_l_or_geninstr(struct ast_block *b, struct lexememarker *loc,
		struct ast_expr *e1, struct ast_expr *e2, struct state *);

struct ast_expr *
ast_block_eqop_geninstr(struct ast_block *b, struct lexememarker *loc,
		struct ast_expr *e, struct state *s)
{
	struct ast_expr *e1 = ast_expr_binary_e1(e),
			*e2 = ast_expr_binary_e2(e);
	switch (ast_expr_binary_op(e)) {
	case BINARY_OP_EQ:
		return ast_block_l_and_geninstr(
			b, loc,
			ast_expr_binary_create(e1, BINARY_OP_LE, e2),
			ast_expr_binary_create(e2, BINARY_OP_LE, e1),
			s
		);
	case BINARY_OP_NE:
		return ast_block_l_or_geninstr(
			b, loc,
			ast_expr_binary_create(e1, BINARY_OP_LT, e2),
			ast_expr_binary_create(e2, BINARY_OP_LT, e1),
			s
		);
	default:
		assert(0);
	}
}

static struct ast_expr *
ast_block_ternary_geninstr(struct ast_block *, struct lexememarker *,
		struct ast_expr *, struct ast_expr *, struct ast_expr *,
		struct state *);

static struct ast_expr *
ast_block_l_and_geninstr(struct ast_block *b, struct lexememarker *loc,
		struct ast_expr *e1, struct ast_expr *e2, struct state *s)
{
	return ast_block_ternary_geninstr(
		/* e1 ? e2 : 0 */
		b, loc, e1, e2, ast_expr_constant_create(0), s
	);
}

static struct ast_expr *
ast_block_l_or_geninstr(struct ast_block *b, struct lexememarker *loc,
		struct ast_expr *e1, struct ast_expr *e2, struct state *s)
{
	return ast_block_ternary_geninstr(
		/* e1 ? 1 : e2 */
		b, loc, e1, ast_expr_constant_create(1), e2, s
	);
}

static struct ast_expr *
ast_block_ternary_geninstr(struct ast_block *b, struct lexememarker *loc,
		struct ast_expr *e1, struct ast_expr *e2, struct ast_expr *e3,
		struct state *s)
{
	struct ast_expr *e1_r = ast_expr_geninstr(e1, loc, b, s),
			*e2_r = ast_expr_geninstr(e2, loc, b, s),
			*e3_r = ast_expr_geninstr(e3, loc, b, s);

	/* we implement e1 ? e2 : e3 as
	 *
	 * 	if (e1)
	 * 		mov tvar, e2;
	 * 	else
	 * 		mov tvar, e3;
	 */

	char *tvar = generate_tempvar(b->tempcount++);
	ast_block_append_stmt(
		b,
		ast_stmt_create_sel(
			loc,
			false,
			e1_r,
			ast_stmt_asm_mov_create(
				loc,
				ast_variable_create(
					dynamic_str(tvar),
					/* 3.3.9 in association with the result
					 * type given in 3.3.8 */
					ast_type_create_int()
				),
				e2_r
			),
			ast_stmt_asm_mov_create(
				loc,
				ast_variable_create(
					dynamic_str(tvar),
					/* 3.3.9 */
					ast_type_create_int()
				),
				e3_r
			)
		)
	);
	return ast_expr_identifier_create(tvar);
}


DEFINE_RESULT_TYPE(struct ast_block *, block, ast_block_destroy, ast_block_res, false)

struct ast_block_res *
ast_block_setupdecide(struct ast_block *old, struct state *s)
{
	struct ast_block *new = ast_block_create(NULL, 0);
	for (int i = 0; i < old->nstmt; i++) {
		struct ast_stmt_res *res = ast_stmt_setupdecide(old->stmt[i], s);
		if (ast_stmt_res_iserror(res)) {
			struct error *err = ast_stmt_res_as_error(res);
			if (error_to_modulate_skip(err)) {
				ast_stmt_res_errorignore(res);
				continue; /* skip */
			} else {
				return ast_block_res_error_create(err);
			}
		}
		ast_block_append_stmt(new, ast_stmt_res_as_stmt(res));
	}
	return ast_block_res_block_create(new);
}
