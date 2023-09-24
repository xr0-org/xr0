#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "ast.h"
#include "block.h"
#include "heap.h"
#include "location.h"
#include "object.h"
#include "stack.h"
#include "util.h"
#include "value.h"

struct location {
	enum location_type type;
	int block;
	struct ast_expr *offset;

	int stack_depth; /* XXX */
};

struct location *
location_create(enum location_type type, int block, struct ast_expr *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = type;
	loc->block = block;
	loc->stack_depth = 0;
	assert(offset);
	loc->offset = offset;
	return loc;
}

void
location_destroy(struct location *loc)
{
	ast_expr_destroy(loc->offset);
	free(loc);
}

static bool
offsetzero(struct location *loc);

char *
location_str(struct location *loc)
{
	struct strbuilder *b = strbuilder_create();
	switch (loc->type) {
	case LOCATION_AUTOMATIC:
		strbuilder_printf(b, "stack:");
		break;
	case LOCATION_DYNAMIC:
		strbuilder_printf(b, "heap:");
		break;
	default:
		assert(false);
	}
	strbuilder_printf(b, "%d", loc->block);
	if (!offsetzero(loc)) {
		char *offset = ast_expr_str(loc->offset);
		strbuilder_printf(b, "+%s", offset);
		free(offset);
	}
	return strbuilder_build(b);
}

static bool
offsetzero(struct location *loc)
{
	struct ast_expr *zero = ast_expr_create_constant(0);
	bool eq = ast_expr_equal(loc->offset, zero);
	ast_expr_destroy(zero);
	return eq;
}

void
location_setdepth(struct location *loc, int depth)
{
	loc->stack_depth = depth;
}

enum location_type
location_type(struct location *loc)
{
	return loc->type;
}

int
location_block(struct location *loc)
{
	return loc->block;
}

struct ast_expr *
location_offset(struct location *loc)
{
	return loc->offset;
}

struct location *
location_copy(struct location *loc)
{
	return location_create(
		loc->type, loc->block, ast_expr_copy(loc->offset)
	);
}

struct location *
location_with_offset(struct location *loc, struct ast_expr *offset)
{
	/* TODO: arithemtically recompute offset */
	assert(offsetzero(loc));

	return location_create(loc->type, loc->block, ast_expr_copy(offset));
}

bool
location_isdeallocand(struct location *loc, struct heap *h)
{
	bool type_equal = loc->type == LOCATION_DYNAMIC;
	block *b = heap_getblock(h, loc->block);
	return type_equal && b;
}

bool
location_equal(struct location *l1, struct location *l2)
{
	return l1->type == l2->type
		&& l1->block == l2->block
		&& ast_expr_equal(l1->offset, l2->offset);
}

bool
location_heap_equivalent(struct location *l1, struct location *l2,
		struct stack *s1, struct stack *s2, struct heap *h1, struct heap *h2)
{
	assert(l1 && l2);
	struct object *obj1 = location_getobject(l1, s1, h1),
		      *obj2 = location_getobject(l2, s2, h2);
	assert(obj1 && obj2);

	return value_heap_equivalent(
		object_value(obj1), object_value(obj2), s1, s2, h1, h2
	);
}

struct object *
location_getobject(struct location *loc, struct stack *stack, struct heap *heap)
{
	block *b = location_getblock(loc, stack, heap);
	if (!b) {
		assert(loc->type == LOCATION_DYNAMIC);
		return NULL;
	}
	return block_observe(b, loc->offset, heap);
}

block *
location_getblock(struct location *loc, struct stack *s, struct heap *h)
{
	switch (loc->type) {
	case LOCATION_AUTOMATIC:
		return stack_getblock(stack_prev(s, loc->stack_depth), loc->block);
	case LOCATION_DYNAMIC:
		return heap_getblock(h, loc->block);
	default:
		assert(false);
	}
}

struct error *
location_dealloc(struct location *loc, struct heap *heap)
{
	if (loc->type != LOCATION_DYNAMIC) {
		return error_create("not heap location");
	}
	return heap_deallocblock(heap, loc->block);
}

struct error *
location_range_dealloc(struct location *loc, struct ast_expr *lw,
		struct ast_expr *up, struct stack *stack, struct heap *heap)
{
	/* TODO: adjust lw, up by loc->offset for nonzero cases */
	assert(offsetzero(loc));

	block *b = location_getblock(loc, stack, heap);
	if (!b) {
		return error_create("cannot get block");
	}

	if (!block_range_aredeallocands(b, lw, up, heap)) {
		assert(false);
		return error_create("some values not allocated");
	}

	return block_range_dealloc(b, lw, up, heap);
}


struct variable {
	struct ast_type *type;
	struct location *loc;
	bool isparam;
};

struct variable *
variable_create(struct ast_type *type, struct stack *stack, bool isparam)
{
	struct variable *v = malloc(sizeof(struct variable));
	v->type = ast_type_copy(type);
	v->loc = stack_newblock(stack, 1); /* XXX: will change to sizeof */
	v->isparam = isparam;
	return v;
}

void
variable_destroy(struct variable *v)
{
	ast_type_destroy(v->type);
	location_destroy(v->loc);
	free(v);
}

char *
variable_str(struct variable *v, struct stack *s, struct heap *h)
{
	struct strbuilder *b = strbuilder_create();
	char *type = ast_type_str(v->type);
	char *loc = location_str(v->loc);
	char *isparam = v->isparam ? "param " : "";
	struct object *obj = location_getobject(v->loc, s, h);
	assert(obj);
	char *obj_str = object_str(obj);
	strbuilder_printf(b, "{%s%s := %s} @ %s", isparam, type, obj_str, loc);
	free(obj_str);
	free(loc);
	free(type);
	return strbuilder_build(b);
}

struct ast_variable *
variable_to_ast(struct variable *v, char *name)
{
	return ast_variable_create(
		dynamic_str(name),
		ast_type_copy(v->type)
	);
}

struct location *
variable_location(struct variable *v)
{
	return v->loc;
}

bool
variable_references(struct variable *v, struct location *loc, struct stack *stack,
		struct heap *heap)
{
	struct object *obj = location_getobject(v->loc, stack, heap);
	assert(obj);
	struct value *val = object_value(obj); 
	if (val && value_type(val) == VALUE_PTR) {
		struct location *vloc = value_location(val);
		if (location_equal(loc, vloc)) {
			return true;
		}
	}
	return false;
}

bool
variable_isparam(struct variable *v)
{
	return v->isparam;
}

bool
variable_heap_equivalent(struct variable *v1, struct variable *v2,
		struct stack *s1, struct stack *s2, struct heap *h1, struct heap *h2)
{
	assert(v1 && v2);

	return location_heap_equivalent(v1->loc, v2->loc, s1, s2, h1, h2);
}
