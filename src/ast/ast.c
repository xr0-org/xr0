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

struct lvalue {
	struct ast_type *t;
	struct object *obj;
};

struct lvalue *
lvalue_create(struct ast_type *t, struct object *obj)
{
	assert(t && obj);

	struct lvalue *l = malloc(sizeof(struct lvalue));
	l->t = t;
	l->obj = obj;
	return l;
}

void
lvalue_destroy(struct lvalue *l)
{
	/* does not destroy object because it belongs to state */
	assert(l);
	ast_type_destroy(l->t);
	free(l);
}

struct ast_type *
lvalue_type(struct lvalue *l)
{
	assert(l);
	return l->t;
}

struct object *
lvalue_object(struct lvalue *l)
{
	assert(l);
	return l->obj;
}

struct rvalue {
	struct ast_type *t;
	struct value *v;
};

struct rvalue *
rvalue_create(struct ast_type *t, struct value *v)
{
	assert(t && v);

	struct rvalue *r = malloc(sizeof(struct rvalue));
	r->t = t;
	r->v = v;
	return r;
}

void
rvalue_destroy(struct rvalue *r)
{
	assert(r);

	ast_type_destroy(r->t);
	value_destroy(r->v);
	free(r);
}

struct ast_type *
rvalue_type(struct rvalue *r)
{
	assert(r);
	return r->t;
}

struct value *
rvalue_value(struct rvalue *r)
{
	assert(r);
	return r->v;
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

DEFINE_RESULT_TYPE(struct ast_expr *, expr, ast_expr_destroy, iresult)
DEFINE_RESULT_TYPE(struct lvalue *, lvalue, lvalue_destroy, l_res)
DEFINE_RESULT_TYPE(struct rvalue *, rvalue, rvalue_destroy, r_res)
