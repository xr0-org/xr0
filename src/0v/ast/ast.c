#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "lex.h"
#include "math.h"
#include "util.h"

#include "expr/expr.c"
#include "topological.c"
#include "block.c"
#include "stmt/stmt.c"
#include "type/type.c"
#include "variable.c"
#include "function/function.c"
#include "externdecl.c"
#include "literals.c"

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

struct result {
	struct value *val;
	struct error *err;
};

struct result *
result_error_create(struct error *err)
{
	assert(err);

	struct result *r = malloc(sizeof(struct result));
	r->val = NULL;
	r->err = err;
	return r;
}

struct result *
result_value_create(struct value *val)
{
	struct result *r = malloc(sizeof(struct result));
	r->val = val;
	r->err = NULL;
	return r;
}

void
result_destroy(struct result *res)
{
	assert(!res->err);
	if (res->val) {
		value_destroy(res->val);
	}
	free(res);
}

bool
result_iserror(struct result *res)
{
	return res->err;
}

struct error *
result_as_error(struct result *res)
{
	assert(res->err);
	return res->err;
}

struct value *
result_as_value(struct result *res)
{
	assert(!res->err);
	return res->val;
}

bool
result_hasvalue(struct result *res)
{
	assert(!result_iserror(res));
	return res->val; /* implicit cast */
}


struct lvalue {
	struct ast_type *t;
	struct object *obj;
};

struct lvalue *
lvalue_create(struct ast_type *t, struct object *obj)
{
	struct lvalue *l = malloc(sizeof(struct lvalue));
	l->t = t;
	l->obj = obj;
	return l;
}

void
lvalue_destroy(struct lvalue *l)
{
	ast_type_destroy(l->t);
	object_destroy(l->obj);
	free(l);
}

struct ast_type *
lvalue_type(struct lvalue *l)
{
	return l->t;
}

struct object *
lvalue_object(struct lvalue *l)
{
	return l->obj;
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

/* XXX: extract to own file */

struct history {
	struct state_arr *states;
};

struct history *
history_create()
{
	return malloc(sizeof(struct history));
}

void
history_destroy(struct history *h)
{
	state_arr_destroy(h->states);
	free(h);
}

void
history_record(struct history *h, int linenumber, struct state *s)
{
	state_arr_appendwithline(h->states, linenumber, s);
}

struct state_arr *
history_getstates(struct history *h, int linenumber)
{
	return state_arr_getlinestates(h->states, linenumber);
}
