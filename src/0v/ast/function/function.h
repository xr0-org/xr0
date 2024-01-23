#ifndef AST_FUNCTION_H
#define AST_FUNCTION_H

struct ast_function;

struct ast_function_arr;

struct ast_function_arr *
ast_function_arr_create();

struct ast_function_arr *
ast_function_arr_copy(struct ast_function_arr *);

void
ast_function_arr_destroy(struct ast_function_arr *);

void
ast_function_arr_append(struct ast_function_arr *, struct ast_function *);

void
ast_function_arr_appendrange(struct ast_function_arr *, struct ast_function_arr *);

int
ast_function_arr_len(struct ast_function_arr *);

struct ast_function **
ast_function_arr_func(struct ast_function_arr *);

struct ast_function_arr *
paths_fromfunction(struct ast_function *f);

#endif
