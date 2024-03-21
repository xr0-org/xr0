#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "function.h"
#include "stmt/stmt.h"
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

struct preconds_result
ast_function_preconditions(struct ast_function *f)
{
	/* XXX: should we allow multiple pre tags */
	return ast_block_preconds(ast_function_abstract(f));	
}

struct ast_function *
ast_function_protostitch(struct ast_function *f, struct externals *ext)
{
	struct ast_function *proto = externals_getfunc(ext, f->name);

	if (proto && proto->abstract) {
		f->abstract = ast_block_copy(proto->abstract);
	}
	/* XXX: leaks */
	return f;
}

struct error *
paths_verify(struct ast_function_arr *paths, struct externals *);

static struct error *
path_absverify_withstate(struct ast_function *f, struct state *);

struct error *
ast_function_initparams(struct ast_function *f, struct state *);

struct error *
ast_function_verify(struct ast_function *f, struct externals *ext)
{
	struct error *err;
	struct state *state = state_create(
		dynamic_str(ast_function_name(f)), ext, ast_function_type(f)
	);
	if ((err = ast_function_initparams(f, state))) {
		return err;
	}
	if ((err = path_absverify_withstate(f, state))) {
		return err;
	}
	state_destroy(state);
	return NULL;
}

static struct error *
path_absverify(struct ast_function *f, struct state *state, int index);

static struct error *
path_absverify_withstate(struct ast_function *f, struct state *state)
{
	struct ast_block *abs = ast_function_abstract(f);

	int ndecls = ast_block_ndecls(abs);
	struct ast_variable **var = ast_block_decls(abs);
	for (int i = 0; i < ndecls; i++) {
		state_declare(state, var[i], false);
	}

	return path_absverify(f, state, 0);
}

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state);

static struct error *
split_paths_absverify(struct ast_function *f, struct state *, int index,
		struct ast_stmt_splits *splits);

static struct error *
path_absverify(struct ast_function *f, struct state *state, int index)
{
	struct error *err = NULL;

	struct ast_block *abs = ast_function_abstract(f);

	int nstmts = ast_block_nstmts(abs);
	struct ast_stmt **stmt = ast_block_stmts(abs);
	for (int i = index; i < nstmts; i++) {
		struct ast_stmt_splits splits = ast_stmt_splits(stmt[i], state);
		if (splits.err) {
			return splits.err;
		}
		if (splits.n) {
			assert(splits.cond);
			return split_paths_absverify(f, state, i, &splits);
		}
		if (ast_stmt_ispre(stmt[i])) {
			continue;
		}
		struct result *res = ast_stmt_absexec(stmt[i], state, true);
		if (result_iserror(res)) {
			return result_as_error(res);
		}	
		/* result_destroy(res); */
	}
	
	/* TODO: verify that `result' is of same type as f->result */
	if ((err = abstract_audit(f, state))) {
		return err;
	}
	return NULL;
}

static struct error *
ast_function_precondsinit(struct ast_function *, struct state *);

static struct error *
inititalise_param(struct ast_variable *v, struct state *);

struct error *
ast_function_initparams(struct ast_function *f, struct state *s)
{
	struct error *err;
	/* declare params and locals in stack frame */	
	int nparams = ast_function_nparams(f);
	struct ast_variable **params = ast_function_params(f);
	for (int i = 0; i < nparams; i++) {
		state_declare(s, params[i], true);
	}

	if ((err = ast_function_precondsinit(f, s))) {
		return err;
	}
	
	for (int i = 0; i < nparams; i++) {
		if ((err = inititalise_param(params[i], s))) {
			return err;
		}
	}
	return NULL;
}

static struct error *
ast_function_precondsinit(struct ast_function *f, struct state *s)
{
	struct preconds_result pre = ast_function_preconditions(f);
	if (pre.err) {
		return pre.err;
	}
	if (!pre.stmt) {
		return NULL;
	}
	struct result *res = ast_stmt_absexec(pre.stmt, s, true);
	if (result_iserror(res)) {
		return result_as_error(res);
	}
	return NULL;
}

static struct error *
inititalise_param(struct ast_variable *param, struct state *state)
{
	char *name = ast_variable_name(param);
	struct ast_type *t = ast_variable_type(param);

	struct object *obj = state_getobject(state, name);
	assert(obj);
	if (object_hasvalue(obj)) {
		/* must on the clump or heap */
		struct value *val = object_as_value(obj);	
	//struct location *loc = value_as_location(val);
	//assert(
	//	location_type(loc) == LOCATION_DEREFERENCABLE ||
	//	location_type(loc) == LOCATION_DYNAMIC
	//);
	} else {
		/* variables that aren't talked about by the preconditions */
		struct value *val = state_vconst(state, t, dynamic_str(name), true);
		object_assign(obj, val);
	}
	return NULL;
}

static struct error *
ast_function_setupabsexec(struct ast_function *f, struct state *state);

static struct error *
path_verify(struct ast_function *, struct state *state, int index,
		struct state *abstract_state);

static struct error *
abstract_auditwithstate(struct ast_function *f, struct state *actual_state,
		struct state *abstract_state);

static struct error *
abstract_audit(struct ast_function *f, struct state *abstract_state)
{
	struct error *err;

	struct state *actual_state = state_create_withprops(
		dynamic_str(ast_function_name(f)),
		state_getext(abstract_state),
		ast_function_type(f),
		state_getprops(abstract_state)
	);
	if ((err = ast_function_initparams(f, actual_state))) {
		assert(false);
	}
	if ((err = ast_function_setupabsexec(f, actual_state))) {
		return err;
	}
	if ((err = abstract_auditwithstate(f, actual_state, abstract_state))) {
		return err;	
	}
	state_destroy(actual_state); /* actual_state handled by caller */ 
	return NULL;
}

static struct error *
ast_function_setupabsexec(struct ast_function *f, struct state *state)
{
	struct error *err;
	int nstmts = ast_block_nstmts(f->abstract);
	struct ast_stmt **stmt = ast_block_stmts(f->abstract);
	for (int i = 0; i < nstmts; i++) {
		if ((err = ast_stmt_setupabsexec(stmt[i], state))) {
			return err;
		}
	}
	return NULL;
}



static struct error *
abstract_auditwithstate(struct ast_function *f, struct state *actual_state,
		struct state *abstract_state)
{
	int ndecls = ast_block_ndecls(f->body);
	struct ast_variable **var = ast_block_decls(f->body);
	for (int i = 0; i < ndecls; i++) {
		state_declare(actual_state, var[i], false);
	}

	return path_verify(f, actual_state, 0, abstract_state);
}

static struct error *
split_path_absverify(struct ast_function *f, struct state *state, int index,
		struct ast_expr *cond);

static struct error *
split_paths_absverify(struct ast_function *f, struct state *state, int index,
		struct ast_stmt_splits *splits)
{
	struct error *err;
	for (int i = 0; i < splits->n; i++) {
		if ((err = split_path_absverify(f, state, index, splits->cond[i]))) {
			return err;
		}
	}
	return NULL;
}

static struct ast_function_arr *
abstract_paths(struct ast_function *f, int index, struct ast_expr *cond);

static struct error *
split_path_absverify(struct ast_function *f, struct state *state, int index, struct ast_expr *cond)
{
	struct error *err = NULL;

	/* XXX: we are just changing function name now, makes sense to split
	 * state */
	struct ast_function_arr *paths = abstract_paths(f, index, cond);
	int n = ast_function_arr_len(paths);
	assert(n == 2);
	struct ast_function **func = ast_function_arr_func(paths);
	for (int i = 0; i < n; i++) {
		struct state *s_copy = state_copywithname(
			state, ast_function_name(func[i])
		);
		struct preresult *r = ast_expr_assume(
			ast_expr_inverted_copy(cond, i==1), s_copy
		);
		if (preresult_iserror(r)) {
			return preresult_as_error(r);
		}
		if (!preresult_iscontradiction(r)) {
			/* only run if no contradiction because "ex falso" */	
			if ((err = path_absverify(func[i], s_copy, index))) {
				return err;
			}
		}
		/*state_destroy(s_copy);*/
	}
	return NULL;
}

static struct error *
split_paths_verify(struct ast_function *f, struct state *actual_state,
		int index, struct ast_stmt_splits *splits, struct state *abstract_state);

static struct error *
path_verify(struct ast_function *f, struct state *actual_state, int index,
		struct state *abstract_state)
{
	struct error *err;

	int nstmts = ast_block_nstmts(f->body);
	struct ast_stmt **stmt = ast_block_stmts(f->body);
	for (int i = index; i < nstmts; i++) {
		struct ast_stmt_splits splits = ast_stmt_splits(
			stmt[i], actual_state
		);
		if (splits.n) {
			return split_paths_verify(
				f, actual_state, i, &splits, abstract_state
			);
		}
		if ((err = ast_stmt_process(stmt[i], actual_state))) {
			return err;
		}
		if (ast_stmt_isterminal(stmt[i], actual_state)) {
			break;
		}
		/* result_destroy(res); */
	}
	if (state_hasgarbage(actual_state)) {
		printf("actual: %s\n", state_str(actual_state));
		return error_create("qed error: garbage on heap");
	}
	
	bool equiv = state_equal(actual_state, abstract_state);
	if (!equiv) {
		printf("actual: %s\n", state_str(actual_state));
		printf("abstract: %s\n", state_str(abstract_state));
		return error_create("qed error: actual and abstract states differ");
	}

	return NULL;
}

static struct error *
split_path_verify(struct ast_function *f, struct state *actual_state,
		int index, struct ast_expr *cond, struct state *abstract_state);

static struct error *
split_paths_verify(struct ast_function *f, struct state *actual_state,
		int index, struct ast_stmt_splits *splits, struct state *abstract_state)
{
	struct error *err;
	for (int i = 0; i < splits->n; i++) {
		err = split_path_verify(
			f, actual_state, index, splits->cond[i], abstract_state
		);
		if (err) {
			return err;
		}
	}
	return NULL;
}

static struct ast_function_arr *
body_paths(struct ast_function *f, int index, struct ast_expr *);

static struct error *
split_path_verify(struct ast_function *f, struct state *actual_state,
		int index, struct ast_expr *cond, struct state *abstract_state)
{
	struct error *err;

	/* create two functions with abstracts and bodies
	 * adjusted accordingly */
	struct ast_function_arr *paths = body_paths(f, index, cond);
	int n = ast_function_arr_len(paths);
	assert(n == 2);
	struct ast_function **func = ast_function_arr_func(paths);
	for (int i = 0; i < n; i++) {
		struct state *actual_copy = state_copywithname(
			actual_state, ast_function_name(func[i])
		);
		struct state *abstract_copy = state_copywithname(
			abstract_state, ast_function_name(func[i])
		);
		struct preresult *r = ast_expr_assume(
			ast_expr_inverted_copy(cond, i==1), actual_copy
		);
		if (preresult_iserror(r)) {
			return preresult_as_error(r);
		}
		if (!preresult_iscontradiction(r)) {
			/* only run if no contradiction because "ex falso" */
			if ((err = path_verify(func[i], actual_copy, index, abstract_copy))) {
				return err;
			}
		}
		/*state_destroy(actual_copy);*/
		/*state_destroy(abstract_copy);*/
	}
	return NULL;
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
		struct result *res = ast_stmt_absexec(stmt[i], state, false);
		if (result_iserror(res)) {
			return res;
		}
		/* result_destroy(res); */
	}

	/* wrap result and return */ 
	struct object *obj = state_getresult(state);
	assert(obj);
	return result_value_create(object_as_value(obj));
}
static void
recurse_buildgraph(struct map *g, struct map *dedup, char *fname, struct externals *ext);

struct map *
ast_function_buildgraph(char *fname, struct externals *ext)
{
	struct map *dedup = map_create(),
		   *g = map_create();

	recurse_buildgraph(g, dedup, fname, ext);

	return g;
}

static void
recurse_buildgraph(struct map *g, struct map *dedup, char *fname, struct externals *ext)
{
	struct map *local_dedup = map_create();

	if (map_get(dedup, fname) != NULL) {
		return;
	}
	map_set(dedup, fname, (void *) true);
	struct ast_function *f = externals_getfunc(ext, fname);
	if (!f) {
		/* TODO: pass up an error */
		fprintf(stderr, "function `%s' is not declared\n", fname);	
		exit(EXIT_FAILURE);
	}
	assert(f);

	if (f->isaxiom) {
		return;
	} 

	/* XXX: look in abstracts */
	/* XXX: handle prototypes */
	assert(f->body);
	struct ast_block *body = f->body;
	int nstmts = ast_block_nstmts(body);
	struct ast_stmt **stmt = ast_block_stmts(body);

	assert(stmt);
	struct string_arr *val = string_arr_create();
	for (int i = 0; i < nstmts; i++) {
		struct string_arr *farr = ast_stmt_getfuncs(stmt[i]);		
		if (!farr) {
			continue;
		}

		char **func = string_arr_s(farr); 
		for (int j = 0; j < string_arr_n(farr); j++) {
			/* avoid duplicates */
			if (map_get(local_dedup, func[j]) != NULL) {
				continue;
			}
			
			struct ast_function *f = externals_getfunc(ext, func[j]);
			if (!f->isaxiom) {
				string_arr_append(val, func[j]);	
			}
			map_set(local_dedup, func[j], (void *) true);

			/* recursively build for other funcs */
			recurse_buildgraph(g, dedup, func[j], ext);
		}
	}

	map_set(g, dynamic_str(fname), val);
}

static char *
split_name(char *name, struct ast_expr *assumption);

static struct ast_function_arr *
abstract_paths(struct ast_function *f, int index, struct ast_expr *cond)
{
	struct ast_function_arr *res = ast_function_arr_create();

	struct ast_function *f_true = ast_function_create(
		f->isaxiom,
		ast_type_copy(f->ret),
		split_name(f->name, cond),
		f->nparam,
		ast_variables_copy(f->nparam, f->param),
		ast_block_copy(f->abstract),
		ast_block_copy(f->body)
	);

	struct ast_expr *inv_assumption = ast_expr_inverted_copy(cond, true);
	struct ast_function *f_false = ast_function_create(
		f->isaxiom,
		ast_type_copy(f->ret),
		split_name(f->name, inv_assumption),
		f->nparam,
		ast_variables_copy(f->nparam, f->param),
		ast_block_copy(f->abstract),
		ast_block_copy(f->body)
	);

	ast_function_arr_append(res, f_true);
	ast_function_arr_append(res, f_false);
	return res;
}

static struct ast_function_arr *
body_paths(struct ast_function *f, int index, struct ast_expr *cond)
{
	struct ast_function_arr *res = ast_function_arr_create();

	struct ast_function *f_true = ast_function_create(
		f->isaxiom,
		ast_type_copy(f->ret),
		split_name(f->name, cond),
		f->nparam,
		ast_variables_copy(f->nparam, f->param),
		ast_block_copy(f->abstract),
		f->body
	);

	struct ast_expr *inv_assumption = ast_expr_inverted_copy(cond, true);
	struct ast_function *f_false = ast_function_create(
		f->isaxiom,
		ast_type_copy(f->ret),
		split_name(f->name, inv_assumption),
		f->nparam,
		ast_variables_copy(f->nparam, f->param),
		ast_block_copy(f->abstract),
		f->body
	);

	ast_function_arr_append(res, f_true);
	ast_function_arr_append(res, f_false);
	return res;
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

#include "arr.c"
