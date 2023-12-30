#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "function.h"

struct ast_function_arr *
paths_fromfunction(struct ast_function *f)
{
	struct ast_function_arr *arr = ast_function_arr_create();
	ast_function_arr_append(arr, ast_function_copy(f));
	return arr;
}
