#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "lex.h"
#include "function.h"
#include "stmt/stmt.h"
#include "intern.h"
#include "object.h"
#include "props.h"
#include "path.h"
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
	char *abs = ast_block_absstr(f->abstract, 1);
	strbuilder_printf(b, ") ~ %s", abs);
	free(abs);
	if (f->body) {
		char *body = ast_block_str(f->body, 1);
		strbuilder_printf(b, "%s", body);
		free(body);
	} else {
		strbuilder_printf(b, ";");
	}
	strbuilder_printf(b, "\n");
	return strbuilder_build(b);
}

void
ast_function_setname(struct ast_function *f, char *name)
{
	free(f->name);
	f->name = name;
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
ast_function_verify(struct ast_function *f, struct externals *ext)
{
	struct path *path = path_create(f, ext);
	while (!path_atend(path)) {
		struct error *err = path_step(path);
		if (err) {
			return err;
		}
	}
	path_destroy(path);
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

	/*if ((err = ast_function_precondsinit(f, s))) {*/
		/*return err;*/
	/*}*/
	
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
	struct error *err = ast_stmt_absprocess(
		pre.stmt, ast_function_name(f), s, true, true
	);
	if (err) {
		struct lexememarker *loc = ast_stmt_lexememarker(pre.stmt); 
		assert(loc);
		char *m = lexememarker_str(loc);
		struct error *e = error_printf(
			"%s:%s: %w", 
			m, ast_function_name(f), err
		);
		free(m);
		return e;
	}
	return NULL;
}

static struct error *
inititalise_param(struct ast_variable *param, struct state *state)
{
	char *name = ast_variable_name(param);
	struct ast_type *t = ast_variable_type(param);

	struct object_res res = state_getobject(state, name);
	assert(!res.err);
	if (object_hasvalue(res.obj)) {
		/* must on the clump or heap */
		//struct value *val = object_as_value(obj);	
		//struct location *loc = value_as_location(val);
		//assert(
		//	location_type(loc) == LOCATION_DEREFERENCABLE ||
		//	location_type(loc) == LOCATION_DYNAMIC
		//);
	} else {
		/* variables that aren't talked about by the preconditions */
		struct value *val = state_vconst(state, t, dynamic_str(name), true);
		object_assign(res.obj, val);
	}
	return NULL;
}


struct error *
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

	char *fname = ast_function_name(f);
	int nstmts = ast_block_nstmts(f->abstract);
	struct ast_stmt **stmt = ast_block_stmts(f->abstract);
	for (int i = 0; i < nstmts; i++) {
		struct error *err = ast_stmt_absprocess(stmt[i], fname, state, true, false);
		if (err) {
			return result_error_create(err);
		}
	}

	/* wrap result and return */ 
	struct object_res res = state_getresult(state);
	assert(!res.err);
	return result_value_create(object_as_value(res.obj));
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

#include "arr.c"
