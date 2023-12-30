#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "function.h"

struct ast_function_arr {
	int n;
	struct ast_function **f;
};

struct ast_function_arr *
ast_function_arr_create()
{
	return calloc(1, sizeof(struct ast_function_arr));
}

struct ast_function_arr *
ast_function_arr_copy(struct ast_function_arr *old)
{
	struct ast_function_arr *new = ast_function_arr_create();
	for (int i = 0; i < old->n; i++) {
		ast_function_arr_append(new, ast_function_copy(old->f[i]));
	}
	return new;
}

void
ast_function_arr_destroy(struct ast_function_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		ast_function_destroy(arr->f[i]);
	}
	free(arr);
}

void
ast_function_arr_append(struct ast_function_arr *arr, struct ast_function *f)
{
	arr->f = realloc(arr->f, sizeof(struct ast_function *) * ++arr->n);
	arr->f[arr->n-1] = f;
}

void
ast_function_arr_appendrange(struct ast_function_arr *arr,
		struct ast_function_arr *range)
{
	for (int i = 0; i < range->n; i++) {
		ast_function_arr_append(arr, range->f[i]);
	}
}

int
ast_function_arr_len(struct ast_function_arr *arr)
{
	return arr->n;
}

struct ast_function **
ast_function_arr_func(struct ast_function_arr *arr)
{
	return arr->f;
}
