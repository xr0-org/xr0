#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "ast.h"
#include "block.h"
#include "clump.h"
#include "static.h"
#include "heap.h"
#include "intern.h"
#include "location.h"
#include "object.h"
#include "stack.h"
#include "state.h"
#include "util.h"
#include "value.h"

struct location {
	enum location_type type;
	union {
		int frame;		/* LOCATION_AUTOMATIC */
	} u;
	int block;
	struct ast_expr *offset;
};

struct location *
location_create_vconst(int block, struct ast_expr *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = LOCATION_VCONST;
	loc->block = block;
	assert(offset);
	loc->offset = offset;
	return loc;
}

struct location *
location_create_dereferencable(int block, struct ast_expr *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = LOCATION_DEREFERENCABLE;
	loc->block = block;
	assert(offset);
	loc->offset = offset;
	return loc;
}

struct location *
location_create_static(int block, struct ast_expr *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = LOCATION_STATIC;
	loc->block = block;
	assert(offset);
	loc->offset = offset;
	return loc;
}

struct location *
location_create_dynamic(int block, struct ast_expr *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = LOCATION_DYNAMIC;
	loc->block = block;
	assert(offset);
	loc->offset = offset;
	return loc;
}

struct location *
location_create_automatic(int frame, int block, struct ast_expr *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = LOCATION_AUTOMATIC;
	loc->u.frame = frame;
	loc->block = block;
	assert(offset);
	loc->offset = offset;
	return loc;
}

struct value *
location_transfigure(struct location *loc, struct state *compare)
{
	switch (loc->type) {
	case LOCATION_AUTOMATIC:
	case LOCATION_DEREFERENCABLE:
		return state_clump(compare);
	case LOCATION_DYNAMIC:
		return state_alloc(compare);
	default:
		assert(false);
	}
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
	case LOCATION_STATIC:
		strbuilder_printf(b, "static:");
		break;
	case LOCATION_AUTOMATIC:
		strbuilder_printf(b, "stack[%d]:", loc->u.frame);
		break;
	case LOCATION_DYNAMIC:
		strbuilder_printf(b, "heap:");
		break;
	case LOCATION_DEREFERENCABLE:
		strbuilder_printf(b, "clump:");
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
	struct ast_expr *zero = ast_expr_constant_create(0);
	bool eq = ast_expr_equal(loc->offset, zero);
	ast_expr_destroy(zero);
	return eq;
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
	switch (loc->type) {
	case LOCATION_STATIC:
		return location_create_static(
			loc->block, ast_expr_copy(loc->offset)
		);
	case LOCATION_VCONST:
		return location_create_vconst(
			loc->block, ast_expr_copy(loc->offset)
		);
	case LOCATION_DEREFERENCABLE:
		return location_create_dereferencable(
			loc->block, ast_expr_copy(loc->offset)
		);
	case LOCATION_AUTOMATIC:
		return location_create_automatic(
			loc->u.frame, loc->block, ast_expr_copy(loc->offset)
		);
	case LOCATION_DYNAMIC:
		return location_create_dynamic(
			loc->block, ast_expr_copy(loc->offset)
		);
	default:
		assert(false);
	}
}

struct location *
location_with_offset(struct location *loc, struct ast_expr *offset)
{
	/* TODO: arithemtically recompute offset */
	assert(offsetzero(loc));

	struct location *copy = location_copy(loc);
	copy->offset = ast_expr_copy(offset);

	/* XXX: leaks */
	return copy;
}

bool
location_tostatic(struct location *loc, struct static_memory *sm)
{
	bool type_equal = loc->type == LOCATION_STATIC;
	struct block *b = static_memory_getblock(sm, loc->block);
	return type_equal && b;
}

bool
location_toheap(struct location *loc, struct heap *h)
{
	bool type_equal = loc->type == LOCATION_DYNAMIC;
	struct block *b = heap_getblock(h, loc->block);
	return type_equal && b;
}

bool
location_tostack(struct location *loc, struct stack *s)
{
	bool type_equal = loc->type == LOCATION_AUTOMATIC;
	/* XXX: should probably check that frame is greater than current frame */
	struct block *b = stack_getblock(s, loc->block);
	return type_equal && b;
}

bool
location_toclump(struct location *loc, struct clump *c)
{
	bool type_equal = loc->type == LOCATION_DEREFERENCABLE;
	struct block *b = clump_getblock(c, loc->block);
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
location_references(struct location *l1, struct location *l2, struct state *s)
{
	if (location_equal(l1, l2)) {
		return true;
	}

	struct block *b = state_getblock(s, l1);
	return b && block_references(b, l2, s);
}

bool
location_isauto(struct location *loc)
{
	return loc->type == LOCATION_AUTOMATIC;
}

bool
location_referencesheap(struct location *l, struct state *s)
{
	if (l->type == LOCATION_DYNAMIC) {
		if (heap_blockisfreed(state_getheap(s), l->block)) {
			return false;
		}
		return true;
	}
	struct object_res res = state_get(s, l, false);
	if (res.err) {
		assert(false);
	}
	return res.obj && object_referencesheap(res.obj, s);
}

static struct block_res
location_auto_getblock(struct location *, struct stack *);

struct block_res
location_getblock(struct location *loc, struct static_memory *sm, struct vconst *v, struct stack *s,
		struct heap *h, struct clump *c)
{
	assert(s);
	switch (loc->type) {
	case LOCATION_STATIC:
		return (struct block_res) {
			.b = static_memory_getblock( sm, loc->block),
			.err = NULL
		};
	case LOCATION_AUTOMATIC:
		return location_auto_getblock(loc, s);	
	case LOCATION_DYNAMIC:
		return (struct block_res) {
			.b = heap_getblock(h, loc->block),
			.err = NULL
		};
	case LOCATION_DEREFERENCABLE:
		return (struct block_res) {
			.b = clump_getblock(c, loc->block),
			.err = NULL
		};
	default:
		assert(false);
	}
}

static struct block_res
location_auto_getblock(struct location *loc, struct stack *s)
{
	struct stack *f = stack_getframe(s, loc->u.frame);
	if (!f) {
		return (struct block_res) {
			.b = NULL,
			.err = error_create("stack frame doesn't exist")
		};
	}
	return (struct block_res) {
		.b = stack_getblock(f, loc->block),
		.err = NULL
	};
}

struct block *
location_getstackblock(struct location *loc, struct stack *s)
{
	assert(loc->type == LOCATION_AUTOMATIC);
	return stack_getblock(s, loc->block);
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
		struct ast_expr *up, struct state *state)
{
	/* TODO: adjust lw, up by loc->offset for nonzero cases */
	assert(offsetzero(loc));

	struct block *b = state_getblock(state, loc);
	if (!b) {
		return error_create("cannot get block");
	}

	if (!block_range_aredeallocands(b, lw, up, state)) {
		printf("block: %s\n", block_str(b));
		printf("lw: %s, up: %s\n", ast_expr_str(lw), ast_expr_str(up));
		assert(false);
		return error_create("some values not allocated");
	}

	return block_range_dealloc(b, lw, up, state);
}
