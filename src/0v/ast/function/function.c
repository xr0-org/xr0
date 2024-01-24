#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "function.h"
#include "intern.h"
#include "object.h"
#include "props.h"
#include "state.h"
#include "type/type.h"
#include "stmt/stmt.h"
#include "ext.h"
#include "util.h"

struct ast_function {
	bool isaxiom;

	struct ast_type *ret;

	char *name;
	/* parameters */
	int nparam;
	struct ast_variable **param;

	struct ast_block *abstract, 
			 *body;
};

struct ast_function *
ast_function_create(
	bool isaxiom,
	struct ast_type *ret,
	char *name, 
	int nparam,
	struct ast_variable **param,
	struct ast_block *abstract, 
	struct ast_block *body)
{
	struct ast_function *f = malloc(sizeof(struct ast_function));
	f->isaxiom = isaxiom;
	f->ret = ret;
	f->name = name;
	f->nparam = nparam;
	f->param = param;
	assert(abstract);
	f->abstract = abstract;
	f->body = body;
	return f;
}

void
ast_function_destroy(struct ast_function *f)
{
	ast_type_destroy(f->ret);
	for (int i = 0; i < f->nparam; i++) {
		ast_variable_destroy(f->param[i]);
	}
	ast_block_destroy(f->abstract);
	if (f->body) {
		ast_block_destroy(f->body);
	}
	free(f->param);
	free(f->name);
	free(f);
}

char *
ast_function_str(struct ast_function *f)
{
	struct strbuilder *b = strbuilder_create();
	if (f->isaxiom) {
		strbuilder_printf(b, "axiom ");
	}
	char *ret = ast_type_str(f->ret);
	strbuilder_printf(b, "%s\n", ret);
	free(ret);
	strbuilder_printf(b, "%s(", f->name);
	for (int i = 0; i < f->nparam; i++) {
		char *v = ast_variable_str(f->param[i]);
		char *space = (i + 1 < f->nparam) ? ", " : "";
		strbuilder_printf(b, "%s%s", v, space);
		free(v);
	}
	char *abs = ast_block_str(f->abstract, "\t");
	strbuilder_printf(b, ") ~ [\n%s]", abs);
	free(abs);
	if (f->body) {
		char *body = ast_block_str(f->body, "\t");
		strbuilder_printf(b, "{\n%s}", body);
		free(body);
	} else {
		strbuilder_printf(b, ";");
	}
	strbuilder_printf(b, "\n");
	return strbuilder_build(b);
}

char *
ast_function_name(struct ast_function *f)
{
	return f->name;
}

struct ast_function *
ast_function_copy(struct ast_function *f)
{
	assert(f);
	struct ast_variable **param = malloc(sizeof(struct ast_variable *) * f->nparam);
	for (int i = 0; i < f->nparam; i++) {
		param[i] = ast_variable_copy(f->param[i]);
	}
	return ast_function_create(
		f->isaxiom,
		ast_type_copy(f->ret),
		dynamic_str(f->name),
		f->nparam,
		param,
		ast_block_copy(f->abstract),
		f->body ? ast_block_copy(f->body) : NULL
	);
}

bool
ast_function_isaxiom(struct ast_function *f)
{
	return f->isaxiom;
}

bool
ast_function_isproto(struct ast_function *f)
{
	return f->abstract && !f->body;
}

bool
ast_function_absisempty(struct ast_function *f)
{
	return ast_block_ndecls(f->abstract) == 0 && ast_block_nstmts(f->abstract) == 0;
}

struct ast_type *
ast_function_type(struct ast_function *f)
{
	return f->ret;
}

struct ast_block *
ast_function_body(struct ast_function *f)
{
	if (!f->body) {
		fprintf(stderr, "cannot find body for `%s'\n", f->name);
	}
	assert(f->body);
	return f->body;
}

struct ast_block *
ast_function_abstract(struct ast_function *f)
{
	assert(f->abstract);
	return f->abstract;
}

int
ast_function_nparams(struct ast_function *f)
{
	return f->nparam;
}

struct ast_variable **
ast_function_params(struct ast_function *f)
{
	return f->param;
}

struct ast_function *
proto_stitch(struct ast_function *f, struct externals *);

struct error *
path_verify_withstate(struct ast_function *f, struct state *, int index);

static struct preresult *
parameterise_state(struct state *s, struct ast_function *f);

struct error *
ast_function_verify(struct ast_function *f, struct externals *ext)
{
	struct ast_function *proto = proto_stitch(f, ext);
	struct state *state = state_create(
		dynamic_str(ast_function_name(f)), ext, ast_function_type(f)
	);

	struct preresult *r = parameterise_state(state, f);
	if (preresult_iserror(r)) {
		return preresult_as_error(r);
	} else if (preresult_iscontradiction(r)) {
		/* ex falso quodlibet */
		return NULL;
	}

	return path_verify_withstate(proto, state, 0);
}

struct ast_function *
proto_stitch(struct ast_function *f, struct externals *ext)
{
	struct ast_function *proto = externals_getfunc(ext, f->name);

	if (proto && proto->abstract) {
		f->abstract = ast_block_copy(proto->abstract);
	}
	/* XXX: leaks */
	return f;
}

struct error *
path_verify(struct ast_function *f, struct state *state, int index);

static struct preresult *
install_props(struct state *s, struct ast_function *f);

struct error *
path_verify_withstate(struct ast_function *f, struct state *state, int index)
{
	printf("%s\n", ast_function_str(f));
	printf("state (before): %s\n", state_str(state));

	struct ast_block *body = ast_function_body(f);

	struct preresult *r = install_props(state, f);
	if (preresult_iserror(r)) {
		return preresult_as_error(r);
	} else if (preresult_iscontradiction(r)) {
		/* ex falso quodlibet */
		return NULL;
	}

	int ndecls = ast_block_ndecls(body);
	struct ast_variable **var = ast_block_decls(body);
	for (int i = 0; i < ndecls; i++) {
		state_declare(state, var[i], false);
	}

	struct error *err = path_verify(f, state, index);
	state_destroy(state);
	return err;
}

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state);

static struct error *
split_paths_verify(struct ast_function *f, struct state *, int index, struct ast_expr *);

struct error *
path_verify(struct ast_function *f, struct state *state, int index)
{
	struct error *err = NULL;

	struct ast_block *body = ast_function_body(f);

	int nstmts = ast_block_nstmts(body);
	struct ast_stmt **stmt = ast_block_stmts(body);
	for (int i = 0; i < nstmts; i++) {
		struct ast_stmt_paths p = ast_stmt_paths(stmt[i], state);
		if (p.cond) {
			return split_paths_verify(f, state, i, p.cond);
		}
		/*printf("state: %s\n", state_str(state));*/
		/*printf("%s\n", ast_stmt_str(stmt[i]));*/
		if ((err = ast_stmt_process(stmt[i], state))) {
			return err;
		}
		if (ast_stmt_isterminal(stmt[i])) {
			break;
		}
	}
	/* TODO: verify that `result' is of same type as f->result */
	if ((err = abstract_audit(f, state))) {
		return error_prepend(err, "qed error: ");
	}
	return NULL;
}

static struct preresult *
parameterise_state(struct state *s, struct ast_function *f)
{
	/* declare params and locals in stack frame */
	struct ast_variable **param = ast_function_params(f);
	int nparams = ast_function_nparams(f);
	for (int i = 0; i < nparams; i++) {
		struct ast_variable *p = param[i];
		state_declare(s, p, true);
		char *name = ast_variable_name(p);
		struct object *obj = state_getobject(s, name);
		assert(obj);
		object_assign(
			obj,
			state_vconst(s, ast_variable_type(p), dynamic_str(name), true)
		);
	}
	return preresult_empty_create();
}

static struct preresult *
install_props(struct state *s, struct ast_function *f) {
	struct ast_block *abs = ast_function_abstract(f);
	int nstmts = ast_block_nstmts(abs);
	struct ast_stmt **stmt = ast_block_stmts(abs);
	for (int i = 0; i < nstmts; i++) {
		struct preresult *r = ast_stmt_preprocess(stmt[i], s);
		if (!preresult_isempty(r)) {
			return r;
		}
	}

	return preresult_empty_create();
}

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state)
{
	if (!state_hasgarbage(actual_state)) {
		printf("actual: %s\n", state_str(actual_state));
		return error_create("garbage on heap");
	}

	struct state *alleged_state = state_create(
		dynamic_str(ast_function_name(f)),
		state_getext(actual_state),
		ast_function_type(f)
	);
	struct preresult *r = parameterise_state(alleged_state, f);
	assert(preresult_isempty(r));
	struct preresult *r2 = install_props(alleged_state, f);
	if (preresult_iscontradiction(r2)) {
		printf("contradition\n");
		return NULL;
	}

	/* mutates alleged_state */
	struct result *res = ast_function_absexec(f, alleged_state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}

	bool equiv = state_equal(actual_state, alleged_state);
	if (!equiv) {
		/* XXX: print states */
		return error_create("actual and alleged states differ");
	}

	state_destroy(alleged_state); /* actual_state handled by caller */ 

	return NULL;
}

static struct ast_function_arr *
split_paths(struct ast_function *f, int index, struct ast_expr *);

static struct error *
split_paths_verify(struct ast_function *f, struct state *state, int index,
		struct ast_expr *cond)
{
	struct error *err = NULL;

	/* create two functions with abstracts and bodies
	 * adjusted accordingly */
	struct ast_function_arr *paths = split_paths(f, index, cond);
	int n = ast_function_arr_len(paths);
	assert(n == 2);
	struct ast_function **func = ast_function_arr_func(paths);
	for (int i = 0; i < n; i++) {
		struct state *s_copy = state_copy(state);
		if ((err = path_verify_withstate(func[i], s_copy, index))) {
			return err;
		}
		/*state_destroy(s_copy);*/
	}
	return NULL;
}

static char *
split_name(char *name, struct ast_expr *assumption);

static struct ast_block *
block_withassumption(struct ast_block *old, struct ast_expr *cond);

static struct ast_block *
split_block_index(struct ast_block *b, int split_index, bool enter);

static struct ast_function_arr *
split_paths(struct ast_function *f, int index, struct ast_expr *cond)
{
	struct ast_function_arr *res = ast_function_arr_create();
	
	struct ast_function *f_true = ast_function_create(
		f->isaxiom,
		ast_type_copy(f->ret),
		split_name(f->name, cond),
		f->nparam,
		ast_variables_copy(f->nparam, f->param),
		block_withassumption(f->abstract, ast_expr_copy(cond)),
		split_block_index(f->body, index, true)
	);

	struct ast_expr *inv_assumption = ast_expr_inverted_copy(cond, true);
	struct ast_function *f_false = ast_function_create(
		f->isaxiom,
		ast_type_copy(f->ret),
		split_name(f->name, inv_assumption),
		f->nparam,
		ast_variables_copy(f->nparam, f->param),
		block_withassumption(f->abstract, inv_assumption),
		split_block_index(f->body, index, false)
	);
	
	ast_function_arr_append(res, f_true);
	ast_function_arr_append(res, f_false);
	return res;
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

	struct ast_block *new = ast_block_create(decl, ndecl, stmt, nstmt);
	return new;
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

struct result *
ast_function_absexec(struct ast_function *f, struct state *state)
{
	int ndecls = ast_block_ndecls(f->abstract);
	if (ndecls) {
		struct ast_variable **var = ast_block_decls(f->abstract);
		for (int i = 0; i < ndecls; i++) {
			state_declare(state, var[i], false);
		}
	}

	int nstmts = ast_block_nstmts(f->abstract);
	struct ast_stmt **stmt = ast_block_stmts(f->abstract);
	for (int i = 0; i < nstmts; i++) {
		struct result *res = ast_stmt_absexec(stmt[i], state);
		if (result_iserror(res)) {
			return res;
		}
		result_destroy(res);
	}

	/* wrap result and return */ 
	struct object *obj = state_getresult(state);
	assert(obj);
	return result_value_create(object_as_value(obj));
}

#include "arr.c"
