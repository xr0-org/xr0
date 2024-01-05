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
	strbuilder_printf(b, "func");
	if (f->isaxiom) {
		strbuilder_printf(b, " <axiom>");
	}
	strbuilder_printf(b, " `%s'", f->name);
	char *ret = ast_type_str(f->ret);
	strbuilder_printf(b, " returns %s ", ret);
	free(ret);
	strbuilder_printf(b, "takes [");
	for (int i = 0; i < f->nparam; i++) {
		char *v = ast_variable_str(f->param[i]);
		char *space = (i + 1 < f->nparam) ? ", " : "";
		strbuilder_printf(b, "%s%s", v, space);
		free(v);
	}
	strbuilder_printf(b, "] has abstract:\n%s", ast_block_str(f->abstract));
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

struct error *
paths_verify(struct ast_function_arr *paths, struct externals *);

struct error *
ast_function_verify(struct ast_function *f, struct externals *ext)
{
	struct ast_function_arr *paths = paths_fromfunction(f);
	struct error *err = paths_verify(paths, ext);
	ast_function_arr_destroy(paths);
	return err;
}

struct error *
path_verify_withstate(struct ast_function *f, struct externals *ext);

struct error *
paths_verify(struct ast_function_arr *paths, struct externals *ext)
{	
	int len = ast_function_arr_len(paths);
	struct ast_function **path = ast_function_arr_func(paths);
	for (int i = 0; i < len; i++) {
		struct error *err = NULL;
		if ((err = path_verify_withstate(path[i], ext))) {
			return err;
		}
	}
	return NULL;
}

struct error *
path_verify(struct ast_function *f, struct state *state, struct externals *);

struct error *
path_verify_withstate(struct ast_function *f, struct externals *ext)
{
	struct state *state = state_create(
		dynamic_str(ast_function_name(f)), ext, ast_function_type(f)
	);
	printf("state: %s\n", state_str(state));
	struct error *err = path_verify(f, state, ext);
	state_destroy(state);
	return err;
}

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state,
		struct externals *);

static struct preresult *
parameterise_state(struct state *s, struct ast_function *f);

struct error *
path_verify(struct ast_function *f, struct state *state, struct externals *ext)
{
	struct error *err = NULL;

	struct ast_block *body = ast_function_body(f);

	struct preresult *r = parameterise_state(state, f);
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

	int nstmts = ast_block_nstmts(body);
	struct ast_stmt **stmt = ast_block_stmts(body);
	for (int i = 0; i < nstmts; i++) {
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
	if ((err = abstract_audit(f, state, ext))) {
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
abstract_audit(struct ast_function *f, struct state *actual_state,
		struct externals *ext)
{
	if (!state_hasgarbage(actual_state)) {
		printf("actual: %s\n", state_str(actual_state));
		return error_create("garbage on heap");
	}

	struct state *alleged_state = state_create(
		dynamic_str(ast_function_name(f)), ext, ast_function_type(f)
	);
	struct preresult *r = parameterise_state(alleged_state, f);
	assert(preresult_isempty(r));

	/* mutates alleged_state */
	struct result *res = ast_function_absexec(f, alleged_state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}

	bool equiv = state_equal(actual_state, alleged_state);
	if (!equiv) {
		/* XXX: print states */
		printf("actual: %s\n", state_str(actual_state));
		printf("alleged: %s\n", state_str(alleged_state));
		return error_create("actual and alleged states differ");
	}

	state_destroy(alleged_state); /* actual_state handled by caller */ 

	return NULL;
}

struct result *
ast_function_absexec(struct ast_function *f, struct state *state)
{
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
#include "paths.c"
