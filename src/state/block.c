#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "block.h"
#include "stack.h"
#include "heap.h"
#include "state.h"
#include "location.h"
#include "object.h"
#include "util.h"
#include "value.h"

struct block {
	struct object_arr *arr;
};

struct block *
block_create()
{
	struct block *b = malloc(sizeof(struct block));
	b->arr = object_arr_create();
	return b;
}

void
block_destroy(struct block *b)
{
	object_arr_destroy(b->arr);
	free(b);
}

struct block *
block_copy(struct block *old)
{
	struct block *new = malloc(sizeof(struct block));
	new->arr = object_arr_copy(old->arr);
	return new;
}

char *
block_str(struct block *block)
{
	struct strbuilder *b = strbuilder_create();
	struct object **obj = object_arr_objects(block->arr);
	int n = object_arr_nobjects(block->arr);
	for (int i = 0; i < n; i++) {
		char *s = object_str(obj[i]);
		strbuilder_printf(b, "%s%s", s, (i + 1 < n) ? ", " : "");
		free(s);
	}
	return strbuilder_build(b);
}

void
block_install(struct block *b, struct object *obj)
{
	assert(object_arr_nobjects(b->arr) == 0);

	object_arr_append(b->arr, obj);
}

struct object *
block_observe(struct block *b, struct ast_expr *offset, struct state *s,
		bool constructive)
{
	int index = object_arr_index(b->arr, offset, s);
	if (index == -1) {
		if (!constructive) {
			return NULL;
		}
		struct object *obj = object_value_create(
			ast_expr_copy(offset), NULL
		);
		object_arr_append(b->arr, obj);
		return obj;
	}

	struct object *obj = object_arr_objects(b->arr)[index];

	if (object_isvalue(obj)) {
		return obj;
	}

	/* range around observand at offset */
	struct ast_expr *lw = ast_expr_copy(offset);
	struct ast_expr *up = ast_expr_binary_create(
		ast_expr_copy(offset),
		BINARY_OP_ADDITION,
		ast_expr_constant_create(1)
	);
 
	/* ordering makes them sequential in heap */
	struct object *upto = object_upto(obj, lw, s);
	struct object *observed = 
		object_value_create(ast_expr_copy(lw), state_alloc(s));
	struct object *from = object_from(obj, up, s);

	ast_expr_destroy(up);
	ast_expr_destroy(lw);

	/* delete current struct block */
	struct error *err = object_dealloc(obj, s);
	assert(!err);
	object_arr_remove(b->arr, index);

	if (upto) {
		object_arr_insert(b->arr, index++, upto);
	}
	object_arr_insert(b->arr, index++, observed);
	if (from) {
		object_arr_insert(b->arr, index, from);
	}

	return observed;
}

bool
block_references(struct block *b, struct location *loc, struct state *s)
{
	int n = object_arr_nobjects(b->arr);
	struct object **obj = object_arr_objects(b->arr);
	for (int i = 0; i < n; i++) {
		if (object_references(obj[i], loc, s)) {
			return true;
		}
	}
	return false;
}

struct error *
block_range_alloc(struct block *b, struct ast_expr *lw, struct ast_expr *up,
		struct heap *heap)
{
	/* XXX: confirm is single object that has never been assigned to  */
	assert(object_arr_nobjects(b->arr) == 0);

	object_arr_append(
		b->arr,
		object_range_create(
			ast_expr_copy(lw),
			range_create(
				ast_expr_binary_create(
					ast_expr_copy(up),
					BINARY_OP_SUBTRACTION,
					ast_expr_copy(lw)
				),
				heap_newblock(heap)
			)
		)
	);

	return NULL;
}

static bool
hack_first_object_is_exactly_bounds(struct block *b, struct ast_expr *lw,
		struct ast_expr *up, struct state *s);

bool
block_range_aredeallocands(struct block *b, struct ast_expr *lw, struct ast_expr *up,
		struct state *s)
{
	if (hack_first_object_is_exactly_bounds(b, lw, up, s)) {
		return true;
	}

	int lw_index = object_arr_index(b->arr, lw, s);
	if (lw_index == -1) {
		return false;
	}

	int up_index = object_arr_index_upperincl(b->arr, up, s);
	if (up_index == -1) {
		return false;
	}
	
	struct object **obj = object_arr_objects(b->arr);
	for (int i = lw_index; i < up_index; i++) {
		if (!object_isdeallocand(obj[i], s)) {
			return false;
		}
		if (!object_contig_precedes(obj[i], obj[i+1], s)) {
			return false;
		}
	}
	/* XXX: do we need to check above that obj[i+1] is a deallocand? */
	assert(object_isdeallocand(obj[up_index], s));
	return true;
}

static bool
hack_first_object_is_exactly_bounds(struct block *b, struct ast_expr *lw,
		struct ast_expr *up, struct state *s)
{
	if (object_arr_nobjects(b->arr) == 0) {
		return false;
	}

	struct object *obj = object_arr_objects(b->arr)[0];

	if (!object_isdeallocand(obj, s)) {
		return false;
	}

	struct ast_expr *same_lw =
		ast_expr_binary_create(lw, BINARY_OP_EQ, object_lower(obj)),
			*same_up =
		ast_expr_binary_create(up, BINARY_OP_EQ, object_upper(obj));

	return state_eval(s, same_lw) && state_eval(s, same_up);
}

struct error *
block_range_dealloc(struct block *b, struct ast_expr *lw, struct ast_expr *up,
		struct state *s)
{
	if (hack_first_object_is_exactly_bounds(b, lw, up, s)) {
		struct error *err = object_dealloc(object_arr_objects(b->arr)[0], s);
		if (err) {
			return err;
		}
		object_arr_remove(b->arr, 0);
		return NULL;
	}

	int lw_index = object_arr_index(b->arr, lw, s);
	if (lw_index == -1) {
		return error_create("lower bound not allocated");
	}

	int up_index = object_arr_index_upperincl(b->arr, up, s);
	if (up_index == -1) {
		return error_create("upper bound not allocated");
	}

	int n = object_arr_nobjects(b->arr);
	struct object **obj = object_arr_objects(b->arr);

	struct object *upto = object_upto(obj[lw_index], lw, s),
		      *from = object_from(obj[up_index], up, s);

	struct object_arr *new = object_arr_create();

	for (int i = 0; i < lw_index; i++) {
		object_arr_append(new, obj[i]);
	}
	if (upto) {
		object_arr_append(b->arr, upto);
	}
	if (from) {
		object_arr_append(b->arr, from);
	}
	for (int i = up_index+1; i < n; i++) {
		object_arr_append(new, obj[i]);
	}

	for (int i = lw_index; i <= up_index; i++) {
		struct error *err = object_dealloc(obj[i], s);
		if (err) {
			return err;
		}
	}

	b->arr = new;

	return NULL;
}

struct block_arr {
	int n;
	struct block **block;
};

struct block_arr *
block_arr_create()
{
	struct block_arr *arr = calloc(1, sizeof(struct block_arr));
	assert(arr);
	return arr;
}

void
block_arr_destroy(struct block_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		block_destroy(arr->block[i]);
	}
	free(arr->block);
	free(arr);
}

struct block_arr *
block_arr_copy(struct block_arr *old)
{
	struct block_arr *new = block_arr_create();
	for (int i = 0; i < old->n; i++) {
		block_arr_append(new, block_copy(old->block[i]));
	}
	return new;
}

struct block **
block_arr_blocks(struct block_arr *arr)
{
	return arr->block;
}

int
block_arr_nblocks(struct block_arr *arr)
{
	return arr->n;
}

/* block_arr_append: Append struct block to array and return index (address). */
int
block_arr_append(struct block_arr *arr, struct block *b)
{
	arr->block = realloc(arr->block, sizeof(struct block_arr) * ++arr->n);
	assert(arr->block);
	int loc = arr->n-1;
	arr->block[loc] = b;
	return loc;
}

void
block_arr_delete(struct block_arr *arr, int address)
{
	assert(false);
}
