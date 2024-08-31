#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "block.h"
#include "heap.h"
#include "location.h"
#include "object.h"
#include "stack.h"
#include "state.h"
#include "util.h"
#include "value.h"
#include "verifier.h"

struct block {
	struct object_arr *arr;
	bool caller;
	int size;
};

struct block *
block_create(int size)
{
	struct block *b = malloc(sizeof(struct block));
	b->arr = object_arr_create();
	b->caller = false;
	b->size = size;
	return b;
}

struct block *
block_callercreate(int size)
{
	struct block *b = malloc(sizeof(struct block));
	b->arr = object_arr_create();
	b->caller = true;
	b->size = size;
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
	new->caller = old->caller;
	new->size = old->size;
	return new;
}

struct block *
block_permuteheaplocs(struct block *old, struct permutation *p)
{
	struct block *new = malloc(sizeof(struct block));
	new->arr = object_arr_create();
	new->caller = old->caller;
	new->size = old->size;

	struct object **obj = object_arr_objects(old->arr);
	int n = object_arr_nobjects(old->arr);
	for (int i = 0; i < n; i++) {
		object_arr_append(new->arr, object_permuteheaplocs(obj[i], p));
	}

	return new;
}

char *
block_str(struct block *block)
{
	struct strbuilder *b = strbuilder_create();
	struct object **obj = object_arr_objects(block->arr);
	int n = object_arr_nobjects(block->arr);
	strbuilder_printf(b, "|%d| ", block->size);
	for (int i = 0; i < n; i++) {
		char *s = object_str(obj[i]);
		strbuilder_printf(b, "%s%s", s, (i + 1 < n) ? " " : "");
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

struct object_res *
block_observe(struct block *b, struct ast_expr *offset, struct state *s,
		bool constructive)
{
	struct tagval_res *res = ast_expr_rangeeval(offset, s);
	if (tagval_res_iserror(res)) {
		return object_res_error_create(tagval_res_as_error(res));
	}
	struct tagval *tv = tagval_res_as_tagval(res);

	struct value *range = tagval_value(tv);
	int lw = value_int_lw(range),
	    up = value_int_up(range);
	if (lw < 0 || b->size < up) {
		return object_res_error_create(
			error_printf("out of bounds")
		);
	}

	if (!value_isconstant(range)) {
		assert(tagval_hastag(tv));
		struct splitinstruct *splits = splitinstruct_create();
		char *tag = tagval_tag(tv);
		for (int i = lw; i < up; i++) {
			struct map *m = map_create();
			map_set(m, tag, number_single_create(i));
			splitinstruct_append(splits, m);
		}
		return object_res_error_create(
			error_verifierinstruct(verifierinstruct_split(splits))
		);
	}

	offset = ast_expr_constant_create(value_as_constant(range));

	int index = object_arr_index(b->arr, offset, s);
	if (index == -1) {
		if (!constructive) {
			return object_res_error_create(
				error_block_observe_noobj()
			);
		}
		struct object *obj = object_value_create(
			ast_expr_copy(offset), NULL
		);
		object_arr_append(b->arr, obj);
		return object_res_object_create(obj);
	}

	return object_res_object_create(object_arr_objects(b->arr)[index]);
}

bool
block_references(struct block *b, struct location *loc, struct state *s,
		struct circuitbreaker *cb)
{
	int n = object_arr_nobjects(b->arr);
	struct object **obj = object_arr_objects(b->arr);
	for (int i = 0; i < n; i++) {
		if (object_references(obj[i], loc, s, cb)) {
			return true;
		}
	}
	return false;
}

bool
block_iscaller(struct block *b)
{
	return b->caller;
}

void
block_undeclare(struct block *b, struct state *s)
{
	struct object_arr *new = object_arr_create();

	int n = object_arr_nobjects(b->arr);
	struct object **object = object_arr_objects(b->arr);
	for (int i = 0; i < n; i++) {
		struct object *obj = object[i];
		struct circuitbreaker *cb = circuitbreaker_create();
		if (object_referencesheap(obj, s, cb)) {
			object_arr_append(new, object_abstractcopy(obj, s));
		}
		circuitbreaker_destroy(cb);
	}
	object_arr_destroy(b->arr);

	b->arr = new;
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

DEFINE_RESULT_TYPE(struct block *, block, block_destroy, block_res, false)

struct permutation {
	struct int_arr *arr;
};

struct permutation *
permutation_create(struct int_arr *arr)
{
	struct permutation *p = malloc(sizeof(struct permutation));
	p->arr = arr;
	return p;
}

struct permutation *
permutation_copy(struct permutation *old)
{
	struct permutation *new = malloc(sizeof(struct permutation));
	new->arr = int_arr_create();
	int *old_arr = int_arr_arr(old->arr);
	int old_len = int_arr_len(old->arr);
	for (int i = 0; i < old_len; i++) {
		int_arr_append(new->arr, old_arr[i]);
	}
	return new;
}

void
permutation_destroy(struct permutation *p)
{
	int_arr_destroy(p->arr);
	free(p);
}

int
permutation_apply(struct permutation *p, int i)
{
	assert(i < int_arr_len(p->arr));
	return int_arr_arr(p->arr)[i];
}

int
permutation_applyinverse(struct permutation *p, int i)
{
	int len = int_arr_len(p->arr);
	int *perm = int_arr_arr(p->arr);
	assert(i < len);
	for (int j = 0; j < len; j++) {
		if (perm[j] == i) {
			return j;
		}
	}
	assert(false);
}

char *
permutation_str(struct permutation *p)
{
	int len = int_arr_len(p->arr);
	int *arr = int_arr_arr(p->arr);

	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "[");
	for (int i = 0; i < len; i++) {
		strbuilder_printf(b, "%d->%d%s", i, arr[i], i+1<len ? " " : "");
	}
	strbuilder_printf(b, "]");
	return strbuilder_build(b);
}
