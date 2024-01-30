#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "lex.h"
#include "math.h"
#include "util.h"
#include "cJSON.h"

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
	struct history *h = malloc(sizeof(struct history));
	h->states = state_arr_create();
	return h;
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

char *
history_str(struct history *h)
{
	struct strbuilder *b = strbuilder_create();
	struct state **states = state_arr_states(h->states);
	strbuilder_printf(b, "history:\n");
	for (int i = 0; i < state_arr_n(h->states); i++) {
		strbuilder_printf(b, "%s\n\n", state_str(states[i]));
	}
	return strbuilder_build(b);
}

char *
history_tojson(struct history *h)
{
	struct cJSON *root = cJSON_CreateArray();
	
	struct state **states = state_arr_states(h->states);
	for (int i = 0; i < state_arr_n(h->states); i++) {
		struct cJSON *entry = cJSON_CreateObject();
		struct cJSON *linenum = cJSON_CreateNumber(state_getlinenum(states[i]));
		struct cJSON *state = cJSON_CreateString(state_str(states[i]));
		cJSON_AddItemToObjectCS(entry, "line", linenum);
		cJSON_AddItemToObjectCS(entry, "state", state);
		cJSON_AddItemToArray(root, entry);
	}
	return cJSON_Print(root);
}
