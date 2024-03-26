#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "util.h"

struct ast_variable {
	char *name;
	struct ast_type *type;
};

struct ast_variable *
ast_variable_create(char *name, struct ast_type *type)
{
	struct ast_variable *v = malloc(sizeof(struct ast_variable));
	v->name = name;
	v->type = type;
	return v;
}

void
ast_variable_destroy(struct ast_variable *v)
{
	ast_type_destroy(v->type);
	free(v->name);
	free(v);
}

struct ast_variable *
ast_variable_copy(struct ast_variable *v)
{
	assert(v);
	return ast_variable_create(
		dynamic_str(v->name), ast_type_copy(v->type)
	);
}

struct ast_variable **
ast_variables_copy(int n, struct ast_variable **v)
{
	assert(v || !n);
	struct ast_variable **new = calloc(n, sizeof(struct variable *));
	for (int i = 0; i < n; i++) {
		new[i] = ast_variable_copy(v[i]);
	}
	return new;
}

char *
ast_variable_str(struct ast_variable *v)
{
	struct strbuilder *b = strbuilder_create();
	char *t = ast_type_str(v->type);
	
	strbuilder_printf(b, "%s %s", t, v->name);
	free(t);
	return strbuilder_build(b);
}

char *
ast_variable_name(struct ast_variable *v)
{
	return v->name;
}

struct ast_type *
ast_variable_type(struct ast_variable *v)
{
	return v->type;
}


struct ast_variable_arr {
	int n;
	struct ast_variable **v;
};

struct ast_variable_arr *
ast_variable_arr_create()
{
	return calloc(1, sizeof(struct ast_variable_arr));
}

void
ast_variable_arr_append(struct ast_variable_arr *arr, struct ast_variable *v)
{
	arr->v = realloc(arr->v, sizeof(struct ast_variable *) * ++arr->n);
	arr->v[arr->n-1] = v;
}

void
ast_variable_arr_destroy(struct ast_variable_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		ast_variable_destroy(arr->v[i]);
	}
	free(arr);
}

int
ast_variable_arr_n(struct ast_variable_arr *arr)
{
	return arr->n;
}

struct ast_variable **
ast_variable_arr_v(struct ast_variable_arr *arr)
{
	return arr->v;
}

struct ast_variable_arr *
ast_variable_arr_copy(struct ast_variable_arr *old)
{
	struct ast_variable_arr *new = ast_variable_arr_create();
	for (int i = 0; i < old->n; i++) {
		ast_variable_arr_append(new, ast_variable_copy(old->v[i]));
	}
	return new;
}
