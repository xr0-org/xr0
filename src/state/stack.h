#ifndef STACK_H
#define STACK_H

#include <stdbool.h>
#include "block.h"

struct stack;

struct stack *
stack_create(struct stack *prev, struct ast_type *ret_type);

struct location *
stack_newblock(struct stack *stack, int size);

void
stack_destroy(struct stack *);

char *
stack_str(struct stack *, struct heap *);

struct stack *
stack_prev(struct stack *, int depth);

void
stack_declare(struct stack *, struct ast_variable *var, bool isparam);

struct ast_variable **
stack_getvariables(struct stack *);

int
stack_nvariables(struct stack *);

struct variable *
stack_getresult(struct stack *);

struct map *
stack_getvarmap(struct stack *);

struct location *
stack_getlocation(struct stack *, char *id);

bool
stack_references(struct stack *, struct heap *, struct location *);

struct ast_expr;

block *
stack_getblock(struct stack *, int address);

#endif
