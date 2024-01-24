#ifndef STACK_H
#define STACK_H

#include <stdbool.h>
#include "block.h"

struct stack;

struct stack *
stack_create(char *name, struct stack *prev, struct ast_type *ret_type);

struct location *
stack_newblock(struct stack *stack);

void
stack_destroy(struct stack *);

struct stack *
stack_copy(struct stack *);

struct stack *
stack_copywithname(struct stack *, char *new_name);

char *
stack_str(struct stack *, struct state *);

struct stack *
stack_prev(struct stack *);

void
stack_declare(struct stack *, struct ast_variable *var, bool isparam);

void
stack_undeclare(struct stack *stack, struct state *state);

struct variable *
stack_getresult(struct stack *);

struct map *
stack_getvarmap(struct stack *);

struct variable *
stack_getvariable(struct stack *s, char *id);

bool
stack_references(struct stack *s, struct location *loc, struct state *state);

struct ast_expr;

struct block *
stack_getblock(struct stack *, int address);

struct ast_type;

struct variable;

struct stack;

struct variable *
variable_create(struct ast_type *type, struct stack *, bool isparam);

void
variable_destroy(struct variable *);

struct variable *
variable_copy(struct variable *);

char *
variable_str(struct variable *var, struct stack *, struct state *);

struct location *
variable_location(struct variable *);

struct ast_type *
variable_type(struct variable *);

bool
variable_references(struct variable *v, struct location *loc, struct state *s);

bool
variable_isparam(struct variable *);

#endif
