#ifndef LOCATION_H
#define LOCATION_H

#include <stdbool.h>
#include "block.h"

struct ast_expr;

enum location_type {
	/* TODO: STATIC */
	LOCATION_AUTOMATIC, LOCATION_DYNAMIC,
};

struct location;

struct location *
location_create(enum location_type type, int block, struct ast_expr *offset);

struct location *
location_copy(struct location *loc);

void
location_destroy(struct location *);

char *
location_str(struct location *);

void
location_setdepth(struct location *loc, int depth);

struct location *
location_with_offset(struct location *loc, struct ast_expr *offset);

struct heap;

bool
location_isdeallocand(struct location *loc, struct heap *h);

bool
location_equal(struct location *loc1, struct location *loc2);


bool
location_heap_equivalent(struct location *loc1, struct location *loc2,
		struct stack *s1, struct stack *s2, struct heap *h1, struct heap *h2);

struct stack;
struct object;

struct object *
location_getobject(struct location *, struct stack *stack, struct heap *heap);

block *
location_getblock(struct location *, struct stack *stack, struct heap *heap);

struct error *
location_dealloc(struct location *, struct heap *);

struct error *
location_range_dealloc(struct location *loc, struct ast_expr *lw,
		struct ast_expr *up, struct stack *stack, struct heap *heap);

struct ast_type;

struct variable;

struct stack;

struct variable *
variable_create(struct ast_type *type, struct stack *stack, bool isparam);

void
variable_destroy(struct variable *);

char *
variable_str(struct variable *, struct stack *, struct heap *);

struct ast_variable *
variable_to_ast(struct variable *, char *name);

struct location *
variable_location(struct variable *);

bool
variable_references(struct variable *, struct location *, struct stack *,
		struct heap *);

bool
variable_isparam(struct variable *);

bool
variable_heap_equivalent(struct variable *v1, struct variable *v2,
		struct stack *s1, struct stack *s2, struct heap *h1, struct heap *h2);

#endif
