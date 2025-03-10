#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "gram_util.h"
#include "gram.tab.h"
#include "ast.h"
#include "command.h"
#include "ext.h"
#include "function.h"
#include "intern.h"
#include "lex.h"
#include "object.h"
#include "state.h"
#include "stmt.h"
#include "type.h"
#include "util.h"
#include "verifier.h"

struct ast_function {
	bool isaxiom;

	struct ast_type *ret;

	char *name;
	/* parameters */
	int nparam;
	struct ast_variable **param;

	struct ast_block *abstract, 
			 *body;
	struct lexememarker *loc;
};

struct ast_function *
ast_function_create(
	bool isaxiom,
	struct ast_type *ret,
	char *name, 
	int nparam,
	struct ast_variable **param,
	struct ast_block *abstract, 
	struct ast_block *body,
	struct lexememarker *loc)
{
	struct ast_function *f = malloc(sizeof(struct ast_function));
	f->isaxiom = isaxiom;
	f->ret = ret;
	f->name = name;
	f->nparam = nparam;
	f->param = param;
	f->abstract = abstract;
	f->body = body;
	f->loc = loc;
	return f;
}

void
ast_function_destroy(struct ast_function *f)
{
	ast_type_destroy(f->ret);
	for (int i = 0; i < f->nparam; i++) {
		ast_variable_destroy(f->param[i]);
	}
	if (f->abstract) {
		ast_block_destroy(f->abstract);
	}
	if (f->body) {
		ast_block_destroy(f->body);
	}
	lexememarker_destroy(f->loc);
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
	strbuilder_printf(b, ")");
	if (f->abstract) {
		char *abs = ast_block_absstr(f->abstract, 1);
		strbuilder_printf(b, " ~ %s", abs);
		free(abs);
	}
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
		f->abstract ? ast_block_copy(f->abstract) : NULL,
		f->body ? ast_block_copy(f->body) : NULL,
		lexememarker_copy(f->loc)
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
	return !f->body;
}

bool
ast_function_isvoid(struct ast_function *f)
{
	return ast_type_isvoid(f->ret);
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
ast_function_protostitch(struct ast_function *f, struct externals *ext)
{
	struct ast_function *proto = externals_getfunc(ext, f->name);

	if (proto && proto->abstract) {
		f->abstract = ast_block_copy(proto->abstract);
	}
	/* XXX: leaks */
	return f;
}

static bool
abstract_hastoplevelreturn(struct ast_function *);

static struct ast_block_res *
generate_abstract(struct ast_function *, struct externals *);

struct error *
ast_function_ensure_hasabstract(struct ast_function *f, struct externals *ext)
{
	if (!abstract_hastoplevelreturn(f)) {
		struct ast_block_res *res = generate_abstract(f, ext);
		if (ast_block_res_iserror(res)) {
			return ast_block_res_as_error(res);
		}
		f->abstract = ast_block_res_as_block(res);
	}
	return NULL;
}

static bool
abstract_hastoplevelreturn(struct ast_function *f)
{
	return f->abstract && ast_block_hastoplevelreturn(f->abstract);
}

static char *
genseqname(struct ast_function *);

static struct ast_block_res *
generate_abstract(struct ast_function *f, struct externals *ext)
{
	struct ast_block *b = f->abstract ? f->abstract : ast_block_create(NULL, 0);
	if (!ast_type_isvoid(f->ret)) {
		struct namedseq *seq = namedseq_create(genseqname(f));
		struct ast_expr *ret = ast_type_rconstgeninstr(
			f->ret, seq, f->loc, b, ext
		);
		namedseq_destroy(seq);
		ast_block_append_stmt(
			b, ast_stmt_create_return(lexememarker_copy(f->loc), ret)
		);
	}
	return ast_block_res_block_create(b);
}

static char *
genseqname(struct ast_function *f)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s:gen", f->name);
	return strbuilder_build(b);
}


struct error *
ast_function_verify(struct ast_function *f, struct externals *ext)
{
	struct verifier *verifier = verifier_create(f, ext);
	while (!verifier_atend(verifier)) {
		struct error *err = verifier_progress(
			verifier, progressor_step()
		);
		if (err) {
			return err;
		}
	}
	verifier_destroy(verifier);
	return NULL;
}

struct error *
ast_function_debug(struct ast_function *f, struct externals *ext, char *sep)
{
	struct verifier *verifier = verifier_create(f, ext);
	while (!verifier_atend(verifier)) {
		d_printf("%s\n", verifier_str(verifier));
		struct error *err = command_next(verifier, sep);
		if (err) {
			return err;
		}
	}
	verifier_destroy(verifier);
	return NULL;
}

static void
inititalise_param(struct ast_variable *v, struct state *);

void
ast_function_initparams(struct ast_function *f, struct state *s)
{
	/* declare params and locals in stack frame */	
	int nparams = ast_function_nparams(f);
	struct ast_variable **params = ast_function_params(f);
	for (int i = 0; i < nparams; i++) {
		state_declare(s, params[i], true);
	}
	for (int i = 0; i < nparams; i++) {
		inititalise_param(params[i], s);
	}
}

static void
inititalise_param(struct ast_variable *param, struct state *state)
{
	char *name = ast_variable_name(param);
	struct ast_type *t = ast_variable_type(param);

	struct object_res *res = state_getobject(state, name);
	struct object *obj = object_res_as_object(res);
	assert(!object_hasvalue(obj)); /* XXX: see git blame */
	struct ast_expr *r = ast_expr_range_create(
		dynamic_str(name),
		ast_expr_rangemin_create(),
		ast_expr_rangemax_create()
	);
	struct value *val = state_rconst(state, t, r, dynamic_str(name), true);
	object_assign(obj, val);
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
