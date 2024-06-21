#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "util.h"

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
ast_block_str_div(struct ast_block *b, int indent_level, char divst, char divend)
{
	assert(indent_level > 0);

	char *indent = indentation(indent_level),
	     *previndent = indentation(indent_level-1);

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
ast_block_render(struct ast_block *b, int index, bool indecls)
{
	struct strbuilder *sb = strbuilder_create();
	for (int i = 0; i < b->nstmt; i++) {
		char *s = ast_stmt_str(b->stmt[i], 2);
		if (i == index && !indecls) {
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
ast_block_isterminal(struct ast_block *b, struct state *s)
{
	for (int i = 0; i < b->nstmt; i++) {
		if (ast_stmt_isterminal(b->stmt[i], s)) {
			return true;
		}
	}
	return false;
}

bool
ast_block_empty(struct ast_block *b)
{
	return b->nstmt == 0;
}

void
ast_block_append_stmt(struct ast_block *b, struct ast_stmt *v)
{
	b->stmt = realloc(b->stmt, sizeof(struct ast_stmt *) * ++b->nstmt);
	b->stmt[b->nstmt-1] = v;
}

struct preconds_result
ast_block_setups(struct ast_block *abs, struct state *state)
{
	struct ast_block *setups = ast_block_create(NULL, 0);
	int nstmts = ast_block_nstmts(abs);
	struct ast_stmt **stmts = ast_block_stmts(abs);
	for (int i = 0; i < nstmts; i++) {
		struct error *err = ast_stmt_buildsetup(stmts[i], state, setups);		
		if (err) {
			return (struct preconds_result) { .b = NULL, .err = err };
		}
	}
	return (struct preconds_result) { .b = setups, .err = NULL };
}

static char *
generate_tempvar(int tempid);

struct ast_expr *
ast_block_call_create(struct ast_block *b, struct lexememarker *loc,
		struct ast_type *rtype, struct ast_expr *expr)
{
	struct ast_stmt *call = ast_stmt_register_call_create(
		loc, ast_expr_copy(expr)
	);
	ast_block_append_stmt(b, call);

	if (!ast_type_isvoid(rtype)) {
		char *tvar = generate_tempvar(b->tempcount++);
		struct ast_stmt *read = ast_stmt_register_mov_create(
			loc,
			ast_variable_create(
				dynamic_str(tvar), ast_type_copy(rtype)
			)
		);
		ast_block_append_stmt(b, read);
		return ast_expr_identifier_create(dynamic_str(tvar));
	}
	return NULL;
}

static char *
generate_tempvar(int tempid)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "<t%d>", tempid);
	return strbuilder_build(b);
}
