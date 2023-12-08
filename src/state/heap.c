#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "ast.h"
#include "block.h"
#include "heap.h"
#include "location.h"
#include "math.h"
#include "object.h"
#include "stack.h"
#include "state.h"
#include "util.h"
#include "value.h"

struct heap {
	struct block_arr *blocks;
};

struct heap *
heap_create()
{
	struct heap *h = malloc(sizeof(struct heap)); assert(h);
	h->blocks = block_arr_create();
	return h;
}

void
heap_destroy(struct heap *h)
{
	block_arr_destroy(h->blocks);
	free(h);
}

struct heap *
heap_copy(struct heap *h)
{
	struct heap *copy = malloc(sizeof(struct heap));	
	copy->blocks = block_arr_copy(h->blocks);
	return copy;
}

static bool
printdelim(struct heap *h, int i);

char *
heap_str(struct heap *h, char *indent)
{
	struct strbuilder *b = strbuilder_create();
	struct block **arr = block_arr_blocks(h->blocks);
	int n = block_arr_nblocks(h->blocks);
	for (int i = 0; i < n; i++) {
		if (block_isfreed(arr[i])) {
			continue;
		}
		char *block = block_str(arr[i]);
		strbuilder_printf(b, "%s%d: %s%s", indent, i, block,
			printdelim(h, i) ? "\n" : "");
		free(block);
	}
	return strbuilder_build(b);
}

static bool
printdelim(struct heap *h, int start)
{
	int n = block_arr_nblocks(h->blocks);
	struct block **b = block_arr_blocks(h->blocks);
	for (int i = start+1; i < n; i++) {
		if (!block_isfreed(b[i])) {
			return true;
		}
	}
	return false;
}


struct block_arr *
heap_blocks(struct heap *h)
{
	assert(h);
	return h->blocks;
}

struct location *
heap_newblock(struct heap *h)
{
	int address = block_arr_append(h->blocks, block_create());

	int n = block_arr_nblocks(h->blocks);
	assert(n > 0);

	return location_create(
		LOCATION_DYNAMIC, address, ast_expr_constant_create(0)
	);
}

struct block *
heap_getblock(struct heap *h, int address)
{
	if (address >= block_arr_nblocks(h->blocks)) {
		return NULL;
	}
	struct block *b = block_arr_blocks(h->blocks)[address];
	if (block_isfreed(b)) {
		return NULL;
	}
	return b;
}

struct error *
heap_deallocblock(struct heap *h, int address)
{
	assert(address < block_arr_nblocks(h->blocks));

	struct block *b = block_arr_blocks(h->blocks)[address];
	if (block_isfreed(b)) {
		return error_create("double free");
	}
	block_free(b);
	return NULL;
}

static bool
block_referenced(struct state *s, int addr);

bool
heap_referenced(struct heap *h, struct state *s)
{
	int n = block_arr_nblocks(h->blocks);
	struct block **b = block_arr_blocks(h->blocks);
	for (int i = 0; i < n; i++) {
		if (!block_isfreed(b[i]) && !block_referenced(s, i)) {
			return false;
		}
	}
	return true;
}

static bool
block_referenced(struct state *s, int addr)
{
	struct location *loc = location_create(
		LOCATION_DYNAMIC, addr, ast_expr_constant_create(0)
	);
	bool referenced = state_references(s, loc); 
	location_destroy(loc);
	return referenced;
}


/* TODO: extract to own file */

struct vconst {
	struct map *varmap;
};

struct vconst *
vconst_create()
{
	struct vconst *v = malloc(sizeof(struct vconst));
	v->varmap = map_create();
	return v;
}

void
vconst_destroy(struct vconst *v)
{
	struct map *m = v->varmap;
	for (int i = 0; i < m->n; i++) {
		value_destroy((struct value *) m->entry[i].value);
	}
	map_destroy(m);
	free(v);
}

struct vconst *
vconst_copy(struct vconst *old)
{
	struct vconst *new = malloc(sizeof(struct vconst));
	new->varmap = map_create();
	struct map *m = old->varmap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(
			new->varmap,
			dynamic_str(e.key),
			value_copy((struct value *) e.value)
		);
	}
	return new;
}

char *
vconst_declare(struct vconst *v, struct value *val)
{
	struct map *m = v->varmap;

	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "$%d", (int) m->n);
	char *s = strbuilder_build(b);

	map_set(m, dynamic_str(s), val);

	return s;
}

struct value *
vconst_get(struct vconst *v, char *id)
{
	return map_get(v->varmap, id);
}

char *
vconst_str(struct vconst *v, char *indent)
{
	struct strbuilder *b = strbuilder_create();
	struct map *m = v->varmap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		char *value = value_str((struct value *) e.value);
		strbuilder_printf(b, "%s%s: %s\n", indent, e.key, value);
		free(value);
	}
	return strbuilder_build(b);
}

bool
vconst_eval(struct vconst *v, struct ast_expr *e)
{
	return ast_expr_eval(e);
}
