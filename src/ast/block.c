#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "util.h"

#define TEMP_PREFIX "`T" /* unreachable from user space */

struct ast_block {
	int ndecl, nstmt;
	struct ast_variable **decl;
	struct ast_stmt **stmt;
	int tempcount;
};

struct ast_block *
ast_block_create(struct ast_variable **decl, int ndecl, 
	struct ast_stmt **stmt, int nstmt)
{
	/* XXX: commented out in exec-branching work */
	/*assert(ndecl > 0 || !decl);*/
	assert(nstmt > 0 || !stmt);

	struct ast_block *b = malloc(sizeof(struct ast_block));
	b->decl = decl;
	b->ndecl = ndecl;
	b->stmt = stmt;
	b->nstmt = nstmt;
	b->tempcount = 0;
	return b;
}

void
ast_block_destroy(struct ast_block *b)
{
	for (int i = 0; i < b->ndecl; i++) {
		ast_variable_destroy(b->decl[i]);
	}
	free(b->decl);
	for (int i = 0; i < b->nstmt; i++) {
		ast_stmt_destroy(b->stmt[i]);
	}
	free(b->stmt);
	free(b);
}

static struct ast_variable **
copy_var_arr(int len, struct ast_variable **);

static struct ast_stmt **
copy_stmt_arr(int len, struct ast_stmt **);

struct ast_block *
ast_block_copy(struct ast_block *b)
{
	assert(b);
	return ast_block_create(
		copy_var_arr(b->ndecl, b->decl),
		b->ndecl,
		copy_stmt_arr(b->nstmt, b->stmt),
		b->nstmt
	);
}

static struct ast_variable **
copy_var_arr(int len, struct ast_variable **var)
{
	assert(len == 0 || var);
	if (len == 0) {
		return NULL;
	}
	struct ast_variable **new = malloc(sizeof(struct ast_variable *) * len); 
	for (int i = 0; i < len; i++) {
		new[i] = ast_variable_copy(var[i]);
	}
	return new;
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
	for (int i = 0; i < b->ndecl; i++) {
		char *s = ast_variable_str(b->decl[i]);
		strbuilder_printf(sb, "%s%s;\n", indent, s);
		free(s);
	}
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
	for (int i = 0; i < b->ndecl; i++) {
		char *s = ast_variable_str(b->decl[i]);
		if (i == index && indecls) {
			strbuilder_printf(sb, "->\t%s;\n", s);
		} else {
			strbuilder_printf(sb, "\t%s;\n", s);
		}
		free(s);
	}
	for (int i = 0; i < b->nstmt; i++) {
		char *s = ast_stmt_str(b->stmt[i], 0);
		if (i == index && !indecls) {
			strbuilder_printf(sb, "->\t%s\n", s);
		} else {
			strbuilder_printf(sb, "\t%s\n", s);
		}
		free(s);
	}
	return strbuilder_build(sb);
}

int
ast_block_ndecls(struct ast_block *b)
{
	return b->ndecl;
}

struct ast_variable **
ast_block_decls(struct ast_block *b)
{
	/* TODO: restore assert or fix structure */
	/*assert(b->ndecl > 0 || !b->decl);*/
	return b->decl;
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

struct preconds_result
ast_block_preconds(struct ast_block *b)
{
	int n = ast_block_nstmts(b);
	struct ast_stmt **stmt = ast_block_stmts(b);
	for (int i = 0; i < n; i++) {
		/* XXX: either enforce one pre block or concat */
		if (ast_stmt_ispre(stmt[i])) {
			struct ast_stmt *preconds = ast_stmt_labelled_stmt(stmt[i]);
			struct error *err = ast_stmt_preconds_validate(preconds);
			if (err) {
				return (struct preconds_result) { .stmt = NULL, .err = err };
			}
			return (struct preconds_result) { .stmt = preconds, .err = NULL };
		}
	}
	return (struct preconds_result) { .stmt = NULL, .err = NULL };
}

void
block_append_decl(struct ast_block *b, struct ast_variable *v)
{
	b->decl = realloc(b->decl, sizeof(struct ast_variable *) * ++b->ndecl);
	b->decl[b->ndecl-1] = v;
}

void
ast_block_append_stmt(struct ast_block *b, struct ast_stmt *v)
{
	b->stmt = realloc(b->stmt, sizeof(struct ast_stmt *) * ++b->nstmt);
	b->stmt[b->nstmt-1] = v;
}

static char *
generate_tempvar(int tempid);

struct ast_expr *
ast_block_call_create(struct ast_block *b, struct lexememarker *loc,
		struct ast_type *rtype, struct ast_expr *expr)
{
	char *tvar = generate_tempvar(b->tempcount++);
	struct ast_stmt *call = ast_stmt_register_call_create(
		loc, ast_expr_copy(expr)
	);
	struct ast_stmt *read = ast_stmt_register_mov_create(
		loc,
		ast_variable_create(dynamic_str(tvar), ast_type_copy(rtype))
	);

	ast_block_append_stmt(b, call);
	ast_block_append_stmt(b, read);
	return ast_expr_identifier_create(dynamic_str(tvar));
}

static char *
generate_tempvar(int tempid)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s%d", dynamic_str(TEMP_PREFIX), tempid);
	return strbuilder_build(b);
}
