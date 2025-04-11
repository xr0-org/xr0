#ifndef AST_FUNCTION_H
#define AST_FUNCTION_H

struct ast_function;

struct externals;

struct map *
ast_function_buildgraph(char *fname, struct externals *ext);

struct ast_function *
ast_function_protostitch(struct ast_function *f, struct externals *ext);

#endif
