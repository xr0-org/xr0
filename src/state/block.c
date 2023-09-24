#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "ast.h"
#include "block.h"
#include "stack.h"
#include "heap.h"
#include "location.h"
#include "object.h"
#include "util.h"
#include "value.h"

block *
block_create(int size)
{
	block *b = object_arr_create();

	struct object *obj = object_create(
		ast_expr_create_constant(0),
		ast_expr_create_constant(size)
	);
	object_arr_append(b, obj);

	return b;
}

void
block_destroy(block *b)
{
	object_arr_destroy(b);
}

char *
block_str(block *block)
{
	struct strbuilder *b = strbuilder_create();
	struct object **object = object_arr_objects(block);
	int n = object_arr_nobjects(block);
	for (int i = 0; i < n; i++) {
		char *obj = object_str(object[i]);
		strbuilder_printf(b,
			"%s%s",
			obj,
			(i + 1 < n) ? ", " : ""
		);
		free(obj);
	}
	return strbuilder_build(b);
}

struct object *
block_observe(block *b, struct ast_expr *offset, struct heap *heap)
{
	int index = object_arr_index(b, offset);
	assert(index != -1); /* TODO: user error? */

	struct object *obj = object_arr_objects(b)[index];

	if (!object_isvirtual(obj)) {
		return obj;
	}

	/* range around observand at offset */
	struct ast_expr *lw = ast_expr_copy(offset);
	struct ast_expr *up = ast_expr_create_binary(
		ast_expr_copy(offset),
		BINARY_OP_ADDITION,
		ast_expr_create_constant(1)
	);
 
	/* ordering makes them sequential in heap */
	struct object *upto = object_upto(obj, lw, heap);
	struct object *observed = object_create(
		ast_expr_copy(lw), ast_expr_copy(up)
	);
	struct location *newloc = heap_newblock(heap, 1);
	object_assign(observed, value_ptr_create(newloc));
	location_destroy(newloc);
	struct object *from = object_from(obj, up, heap);

	ast_expr_destroy(up);
	ast_expr_destroy(lw);

	/* delete current block */
	struct location *loc = value_as_virt(object_value(obj));
	struct error *err = location_dealloc(loc, heap);
	assert(!err);
	object_arr_delete(b, index);

	if (upto) {
		object_arr_insert(b, index++, upto);
	}
	object_arr_insert(b, index++, observed);
	if (from) {
		object_arr_insert(b, index, from);
	}

	return observed;
}

struct error *
block_range_alloc(block *b, struct ast_expr *lw, struct ast_expr *up,
		struct heap *heap)
{
	/* XXX: confirm is single object that has never been assigned to  */
	assert(object_arr_nobjects(b) == 1);
	assert(!object_value(object_arr_objects(b)[0]));

	object_arr_delete(b, 0);

	struct object *obj = object_create(ast_expr_copy(lw), ast_expr_copy(up));
	object_assign(obj, value_virt_create(heap_newblock(heap, 1)));
	object_arr_append(b, obj);

	return NULL;
}

static bool
object_isdeallocand(struct object *obj, struct heap *h);

bool
block_range_aredeallocands(block *b, struct ast_expr *lw, struct ast_expr *up,
		struct heap *h)
{
	assert(!ast_expr_equal(lw, up));

	int lw_index = object_arr_index(b, lw);
	if (lw_index == -1) {
		assert(false);
		return false;
	}

	int up_index = object_arr_index_upperincl(b, up);
	if (up_index == -1) {
		printf("%s not in %s\n", ast_expr_str(up), block_str(b));
		assert(false);
		return false;
	}
	
	struct object **object = object_arr_objects(b);
	for (int i = lw_index; i < up_index; i++) {
		if (!object_isdeallocand(object[i], h)) {
			assert(false);
			return false;
		}
		if (!object_contig_precedes(object[i], object[i+1])) {
			assert(false);
			return false;
		}
	}
	return true;
}

static bool
object_isdeallocand(struct object *obj, struct heap *h)
{
	assert(false);
	struct value *val = object_value(obj);
	if (!val) { /* uninitalized */
		return false;
	}
	if (value_type(val) != (VALUE_PTR || VALUE_VIRTUAL)) {
		return false;
	}
	return location_isdeallocand(value_location(val), h);
}

struct error *
block_range_dealloc(block *b, struct ast_expr *lw, struct ast_expr *up,
		struct heap *heap)
{
	int lw_index = object_arr_index(b, lw);
	if (lw_index == -1) {
		return error_create("lower bound not allocated");
	}

	int up_index = object_arr_index_upperincl(b, up);
	if (up_index == -1) {
		return error_create("upper bound not allocated");
	}

	struct object **object = object_arr_objects(b);

	struct object *upto = object_upto(object[lw_index], lw, heap),
		      *from = object_from(object[up_index], up, heap);

	for (int i = lw_index; i <= up_index; i++) {
		struct location *loc = value_location(object_value(object[i]));
		struct error *err = location_dealloc(loc, heap);
		if (err) {
			return err;
		}
		object_arr_delete(b, i);
	}

	if (upto) {
		object_arr_insert(b, lw_index++, upto);
	}
	if (from) {
		object_arr_insert(b, lw_index, from);
	}

	return NULL;
}


struct block_arr {
	int n;
	block **block;
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

block **
block_arr_blocks(struct block_arr *arr)
{
	return arr->block;
}

int
block_arr_nblocks(struct block_arr *arr)
{
	return arr->n;
}

/* block_arr_append: Append block to array and return index (address). */
int
block_arr_append(struct block_arr *arr, block *b)
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
