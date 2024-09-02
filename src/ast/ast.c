#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "function.h"
#include "intern.h"
#include "lex.h"
#include "math.h"
#include "object.h"
#include "state.h"
#include "topological.h"
#include "util.h"
#include "value.h"

struct ast *
ast_create(struct ast_externdecl *decl)
{
	struct ast *node = calloc(1, sizeof(struct ast));
	return ast_append(node, decl);
}

void
ast_destroy(struct ast *node)
{
	for (int i = 0; i < node->n; i++) {
		ast_externdecl_destroy(node->decl[i]);
	}
	free(node->decl);
	free(node);
}

struct ast *
ast_append(struct ast *node, struct ast_externdecl *decl)
{
	node->decl = realloc(node->decl,
		sizeof(struct ast_externdecl *) * ++node->n);
	node->decl[node->n-1] = decl;
	return node;
}

struct eval {
	struct ast_type *t;
	bool islval;
	union {
		struct location *l;
		struct value *v;
	};
};

static struct eval *
eval_create(struct ast_type *t)
{
	struct eval *e = malloc(sizeof(struct eval));
	assert(e);
	e->t = t;
	return e;
}

struct eval *
eval_lval_create(struct ast_type *t, struct location *l)
{
	struct eval *e = eval_create(t);
	e->l = l;
	e->islval = true;
	return e;
}

struct eval *
eval_rval_create(struct ast_type *t, struct value *v)
{
	struct eval *e = eval_create(t);
	e->v = v;
	e->islval = false;
	return e;
}

void
eval_destroy(struct eval *e)
{
	ast_type_destroy(e->t);
	if (e->islval) {
		location_destroy(e->l);
	} else {
		value_destroy(e->v);
	}
	free(e);
}

char *
eval_str(struct eval *e)
{
	struct strbuilder *b = strbuilder_create();
	char *type = ast_type_str(e->t);
	strbuilder_printf(b, "{ %s := ", type);
	free(type);
	if (e->islval) {
		char *loc = location_str(e->l);
		strbuilder_printf(b, "%s (l)", loc);
		free(loc);
	} else {
		char *v = value_str(e->v);
		strbuilder_printf(b, "%s (v)", v);
		free(v);
	}
	strbuilder_printf(b, " }");
	return strbuilder_build(b);
}

struct ast_type *
eval_type(struct eval *e)
{
	return e->t;
}

bool
eval_islval(struct eval *e)
{
	return e->islval;
}

struct location *
eval_as_lval(struct eval *e)
{
	assert(eval_islval(e));
	return e->l;
}

bool
eval_isrval(struct eval *e)
{
	return !e->islval;
}

struct value *
eval_as_rval(struct eval *e)
{
	assert(eval_isrval(e));
	return e->v;
}

struct value_res *
eval_to_value(struct eval *e, struct state *s)
{
	if (eval_isrval(e)) {
		return value_res_value_create(eval_as_rval(e));
	}
	struct object_res *obj_res = eval_to_object(e, s, false);
	if (object_res_iserror(obj_res)) {
		return value_res_error_create(object_res_as_error(obj_res));
	}
	if (!object_res_hasobject(obj_res)) {
		/* XXX: lval but no object? I guess cause of constructive concept */
		return value_res_empty_create();
	}
	struct value *v = object_as_value(object_res_as_object(obj_res));
	assert(v);
	return value_res_value_create(v);
}

struct object_res *
eval_to_object(struct eval *e, struct state *s, bool constructive)
{
	assert(eval_islval(e));
	struct object_res *obj_res = state_get(s, eval_as_lval(e), constructive);
	if (object_res_iserror(obj_res)) {
		struct error *err = object_res_as_error(obj_res);
		if (error_to_state_get_no_block(err)
			|| error_to_block_observe_noobj(err)) {
			/* suppress empty block and no object errors */
			object_res_errorignore(obj_res);
		} else {
			return obj_res; /* return with error */
		}
	}
	return obj_res;
}


struct preresult {
	bool iscontradiction;
	struct error *err;
};

struct preresult *
preresult_empty_create()
{
	return calloc(1, sizeof(struct preresult));
}

struct preresult *
preresult_error_create(struct error *err)
{
	assert(err);

	struct preresult *r = preresult_empty_create();
	r->err = err;
	return r;
}

struct preresult *
preresult_contradiction_create()
{
	struct preresult *r = preresult_empty_create();
	r->iscontradiction = true;
	return r;
}

void
preresult_destroy(struct preresult *r)
{
	assert(!r->err);

	free(r);
}

bool
preresult_isempty(struct preresult *r)
{
	return !(r->iscontradiction || r->err);
}

bool
preresult_iserror(struct preresult *r)
{
	return r->err;
}

struct error *
preresult_as_error(struct preresult *r)
{
	assert(r->err);

	return r->err;
}

bool
preresult_iscontradiction(struct preresult *r)
{
	return r->iscontradiction;
}

struct string_arr *
ast_topological_order(char *fname, struct externals *ext)
{
	return topological_order(fname, ext);
}

struct ast_function *
ast_protostitch(struct ast_function *f, struct externals *ext)
{
	return ast_function_protostitch(f, ext);
}

DEFINE_RESULT_TYPE(struct ast_expr *, expr, ast_expr_destroy, iresult, false)
DEFINE_RESULT_TYPE(struct eval *, eval, eval_destroy, e_res, false)

struct namedseq {
	int count;
	char *name;
};

struct namedseq *
namedseq_create(char *name)
{
	struct namedseq *seq = malloc(sizeof(struct namedseq));
	assert(seq);
	seq->count = 0;
	seq->name = name;
	return seq;
}

char *
namedseq_next(struct namedseq *seq)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s:%d", seq->name, seq->count++);
	return strbuilder_build(b);
}

void
namedseq_destroy(struct namedseq *seq)
{
	free(seq->name);
	free(seq);
}
