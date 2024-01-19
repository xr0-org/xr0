#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "function.h"
#include "stmt/stmt.h"
#include "util.h"

static struct ast_function_arr *
abstract_paths(struct ast_function *f);

static struct ast_function_arr *
body_paths(struct ast_function *f);

struct ast_function_arr *
paths_fromfunction(struct ast_function *f)
{
	struct ast_function_arr *arr = ast_function_arr_create();

	struct ast_function_arr *abs_paths = abstract_paths(f);
	int abs_len = ast_function_arr_len(abs_paths);
	struct ast_function **abs_f = ast_function_arr_func(abs_paths);
	for (int i = 0; i < abs_len; i++) {
		struct ast_function_arr *body = body_paths(abs_f[i]);
		ast_function_arr_appendrange(arr, ast_function_arr_copy(body));
		ast_function_arr_destroy(body);
	}
	ast_function_arr_destroy(abs_paths);

	return arr;
}

static int
findsel(struct ast_block *b);

static struct ast_function *
abstract_immediate_split(struct ast_function *f, int i, bool enter);

static struct ast_function_arr *
abstract_paths(struct ast_function *f)
{
	struct ast_function_arr *arr = ast_function_arr_create();
	int sel = findsel(ast_function_abstract(f));
	if (sel != -1) {
		struct ast_function *t_p = abstract_immediate_split(f, sel, true),
				    *f_p = abstract_immediate_split(f, sel, false);

		struct ast_function_arr *t_paths = abstract_paths(t_p),
					*f_paths = abstract_paths(f_p);

		ast_function_arr_appendrange(arr, ast_function_arr_copy(t_paths));
		ast_function_arr_appendrange(arr, ast_function_arr_copy(f_paths));

		ast_function_arr_destroy(f_paths);
		ast_function_arr_destroy(t_paths);

		ast_function_destroy(f_p);
		ast_function_destroy(t_p);

	} else {
		ast_function_arr_append(arr, ast_function_copy(f));
	}
	return arr;
}

static struct ast_function *
immediate_split(struct ast_function *f, int i, bool enter);

static struct ast_function_arr *
body_paths(struct ast_function *f)
{
	struct ast_function_arr *arr = ast_function_arr_create();

	int sel = findsel(ast_function_body(f));
	if (sel != -1) {
		struct ast_function *t_p = immediate_split(f, sel, true),
				    *f_p = immediate_split(f, sel, false);

		struct ast_function_arr *t_paths = body_paths(t_p),
					*f_paths = body_paths(f_p);

		ast_function_arr_appendrange(arr, ast_function_arr_copy(t_paths));
		ast_function_arr_appendrange(arr, ast_function_arr_copy(f_paths));

		ast_function_arr_destroy(f_paths);
		ast_function_arr_destroy(t_paths);

		ast_function_destroy(f_p);
		ast_function_destroy(t_p);
	} else {
		ast_function_arr_append(arr, ast_function_copy(f));
	}

	return arr;
}

static int
findsel(struct ast_block *b)
{
	struct ast_stmt **stmt = ast_block_stmts(b);
	int nstmts = ast_block_nstmts(b);
	for (int i = 0; i < nstmts; i++) {
		if (ast_stmt_isselection(stmt[i])) {
			return i;
		}
	}
	return -1;
}

static char *
split_name(char *name, struct ast_expr *assumption);

static struct ast_block *
block_withassumption(struct ast_block *b, struct ast_expr *assumption);

static struct ast_block *
split_block_index(struct ast_block *b, int split_index, bool enter);

static struct ast_function *
abstract_immediate_split(struct ast_function *f, int split_index, bool enter)
{
	struct ast_block *abs = ast_function_abstract(f),
			 *body = ast_function_body(f);

	int nparam = ast_function_nparams(f);
	struct ast_variable **params = ast_function_params(f);

	/* TODO: check that cond is accessible in both abstract and body */
	struct ast_expr *assumption = ast_expr_inverted_copy(
		ast_stmt_sel_cond(ast_block_stmts(abs)[split_index]), !enter
	);

	return ast_function_create(
		false,
		ast_type_copy(ast_function_type(f)),
		split_name(ast_function_name(f), assumption),
		ast_function_nparams(f),
		ast_variables_copy(nparam, params),
		block_withassumption(
			split_block_index(abs, split_index, enter),
			assumption
		),
		ast_block_copy(body)
	);
}

static struct ast_function *
immediate_split(struct ast_function *f, int split_index, bool enter)
{
	struct ast_block *abs = ast_function_abstract(f),
			 *body = ast_function_body(f);

	int nparam = ast_function_nparams(f);
	struct ast_variable **params = ast_function_params(f);

	/* TODO: check that cond is accessible in both abstract and body */
	struct ast_expr *assumption = ast_expr_inverted_copy(
		ast_stmt_sel_cond(ast_block_stmts(body)[split_index]), !enter
	);

	return ast_function_create(
		false,
		ast_type_copy(ast_function_type(f)),
		split_name(ast_function_name(f), assumption),
		ast_function_nparams(f),
		ast_variables_copy(nparam, params),
		block_withassumption(abs, assumption),
		split_block_index(body, split_index, enter)
	);
}

static char *
split_name(char *name, struct ast_expr *assumption)
{
	struct strbuilder *b = strbuilder_create();
	char *assumption_str = ast_expr_str(assumption);
	strbuilder_printf(b, "%s | %s", name, assumption_str);
	free(assumption_str);
	return strbuilder_build(b);
}


static struct ast_block *
block_withassumption(struct ast_block *old, struct ast_expr *cond)
{
	int ndecl = ast_block_ndecls(old);
	struct ast_variable **old_decl = ast_block_decls(old);
	struct ast_variable **decl = malloc(sizeof(struct ast_variable *) * ndecl); 
	for (int i = 0; i < ndecl; i++) {
		decl[i] = ast_variable_copy(old_decl[i]);
	}

	int old_nstmt = ast_block_nstmts(old);
	struct ast_stmt **old_stmt = ast_block_stmts(old);
	int nstmt = old_nstmt+1;
	struct ast_stmt **stmt = malloc(sizeof(struct ast_stmt *) * nstmt);
	for (int i = 0; i < old_nstmt; i++) {
		stmt[i+1] = ast_stmt_copy(old_stmt[i]);
	}
	/* assume: cond; */
	stmt[0] = ast_stmt_create_labelled(
		NULL, dynamic_str("assume"), ast_stmt_create_expr(NULL, cond)
	);

	return ast_block_create(
		ndecl ? decl : NULL, ndecl,
		nstmt ? stmt : NULL, nstmt
	);
}

struct ast_stmt *
choose_split_path(struct ast_stmt *stmt, bool should_split, bool enter);

struct ast_stmt_arr {
	int n;
	struct ast_stmt **stmt;
};

static void
stmt_arr_appendbody(struct ast_stmt_arr *arr, struct ast_stmt *body);

static struct ast_block *
split_block_index(struct ast_block *b, int split_index, bool enter)
{
	int nstmts = ast_block_nstmts(b);
	struct ast_stmt **old_stmt = ast_block_stmts(b);

	struct ast_stmt_arr arr = { 0, NULL };
	for (int i = 0; i < nstmts; i++) {
		struct ast_stmt *s = choose_split_path(
			old_stmt[i], i == split_index, enter
		);
		if (!s) {
			continue;
		}
		stmt_arr_appendbody(&arr, s);
	}

	int ndecl = ast_block_ndecls(b);
	struct ast_variable **old_decl = ast_block_decls(b);
	struct ast_variable **decl =
		old_decl
		? ast_variables_copy(ndecl, old_decl)
		: NULL;
	return ast_block_create(
		decl, ndecl,
		arr.stmt, arr.n
	);
}

struct ast_stmt *
choose_split_path(struct ast_stmt *stmt, bool should_split, bool enter)
{
	if (should_split) {
		return enter ? ast_stmt_sel_body(stmt) : NULL;
	}
	return stmt;
}

static void
stmt_arr_appendbody(struct ast_stmt_arr *arr, struct ast_stmt *body)
{
	/* TODO: carefully sift through all ast_stmt_kinds */
	if (ast_stmt_kind(body) == STMT_COMPOUND) {
		struct ast_block *b = ast_stmt_as_block(body);
		int nstmts = ast_block_nstmts(b);
		struct ast_stmt **stmt = ast_block_stmts(b);
		for (int i = 0; i < nstmts; i++) {
			stmt_arr_appendbody(arr, stmt[i]);
		}
	} else {
		arr->stmt = realloc(arr->stmt, sizeof(struct ast_stmt *) * ++arr->n);
		arr->stmt[arr->n-1] = ast_stmt_copy(body);
	}
}
