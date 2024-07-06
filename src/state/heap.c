#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "block.h"
#include "heap.h"
#include "intern.h"
#include "location.h"
#include "math.h"
#include "object.h"
#include "stack.h"
#include "state.h"
#include "util.h"
#include "value.h"

struct heap {
	struct block_arr *blocks;
	bool *freed;  /* array of same length as blocks */
};

struct heap *
heap_create()
{
	struct heap *h = malloc(sizeof(struct heap)); assert(h);
	h->blocks = block_arr_create();
	h->freed = NULL;
	return h;
}

void
heap_destroy(struct heap *h)
{
	block_arr_destroy(h->blocks);
	free(h->freed);
	free(h);
}

struct heap *
heap_copy(struct heap *h)
{
	struct heap *copy = malloc(sizeof(struct heap));	
	copy->blocks = block_arr_copy(h->blocks);

	int n = block_arr_nblocks(h->blocks);
	bool *freed_copy = malloc(sizeof(bool) * n);
	for (int i = 0; i < n; i++) {
		freed_copy[i] = h->freed[i];	
	}
	copy->freed = freed_copy;
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
		if (h->freed[i]) {
			continue;
		}
		char *block = block_str(arr[i]);
		strbuilder_printf(
			b, "%s%d: %s%s",
			indent, i, (block_iscaller(arr[i]) ? "" : ""),
			block
		);
		if (printdelim(h, i)) {
			strbuilder_printf(b, "\n");
		}
		free(block);
	}
	return strbuilder_build(b);
}

static bool
printdelim(struct heap *h, int start)
{
	int n = block_arr_nblocks(h->blocks);
	for (int i = start+1; i < n; i++) {
		if (!h->freed[i]) {
			return true;
		}
	}
	return false;
}

struct heap *
heap_permute(struct heap *old, struct permutation *p)
{
	struct heap *new = malloc(sizeof(struct heap));
	int n = block_arr_nblocks(old->blocks);
	struct block **b = block_arr_blocks(old->blocks);
	new->blocks = block_arr_create();
	new->freed = malloc(sizeof(bool) * n);
	for (int i = 0; i < n; i++) {
		int perm_i = permutation_apply(p, i);
		new->freed[i] = old->freed[perm_i];
		block_arr_append(
			new->blocks, block_permuteheaplocs(b[perm_i], p)
		);
	}
	return new;
}

/* foundmap: return int map of the given size with the values in the array set
 * to 1, and all the others to 0. */
static int *
foundmap(struct int_arr *, int size);

void
heap_fillorder(struct heap *h, struct int_arr *arr)
{
	int n = block_arr_nblocks(h->blocks);
	int *found = foundmap(arr, n);
	for (int i = 0; i < n; i++) {
		if (!found[i]) {
			int_arr_append(arr, i);
		}
	}
	free(found);
}

static int *
foundmap(struct int_arr *arr, int size)
{
	int arr_len = int_arr_len(arr);
	int *arr_v = int_arr_arr(arr);
	assert(arr_len <= size);

	int *m = calloc(size, sizeof(int));
	for (int i = 0; i < arr_len; i++) {
		assert(arr_v[i] < size);
		m[arr_v[i]] = 1;
	}
	return m;
}

struct block_arr *
heap_blocks(struct heap *h)
{
	assert(h);
	return h->blocks;
}

struct location *
heap_newblock(struct heap *h, int size)
{
	int address = block_arr_append(h->blocks, block_create(size));

	int n = block_arr_nblocks(h->blocks);
	assert(n > 0);
	h->freed = realloc(h->freed, sizeof(bool) * n);
	h->freed[address] = false;
	return location_create_dynamic(
		address, offset_create(ast_expr_constant_create(0))
	);
}

struct location *
heap_newcallerblock(struct heap *h, int size)
{
	int address = block_arr_append(h->blocks, block_callercreate(size));

	int n = block_arr_nblocks(h->blocks);
	assert(n > 0);
	h->freed = realloc(h->freed, sizeof(bool) * n);
	h->freed[address] = false;
	return location_create_dynamic(
		address, offset_create(ast_expr_constant_create(0))
	);
}

struct block *
heap_getblock(struct heap *h, int address)
{
	if (address >= block_arr_nblocks(h->blocks)) {
		return NULL;
	}
	if (h->freed[address]) {
		return NULL;
	}
	return block_arr_blocks(h->blocks)[address];
}

struct error *
heap_deallocblock(struct heap *h, int address)
{
	assert(address < block_arr_nblocks(h->blocks));

	if (h->freed[address]) {
		return error_printf("double free");
	}
	h->freed[address] = true;
	return NULL;
}

bool
heap_blockisfreed(struct heap *h, int address)
{
	return h->freed[address];
}

void
heap_undeclare(struct heap *h, struct state *s)
{
	int n = block_arr_nblocks(h->blocks);
	struct block **b = block_arr_blocks(h->blocks);
	for (int i = 0; i < n; i++) {
		if (!h->freed[i]) {
			block_undeclare(b[i], s);
		}
	}
}

static bool
block_referenced(struct state *s, int addr);

bool
heap_referenced(struct heap *h, struct state *s)
{
	int n = block_arr_nblocks(h->blocks);
	for (int i = 0; i < n; i++) {
		struct block *b = block_arr_blocks(h->blocks)[i];
		if (!h->freed[i] && !block_iscaller(b) && !block_referenced(s, i)) {
			return false;
		}
	}
	return true;
}

static bool
block_referenced(struct state *s, int addr)
{
	struct location *loc = location_create_dynamic(
		addr, offset_create(ast_expr_constant_create(0))
	);
	bool return_references = state_returnreferences(s, loc);
	if (!return_references) {
		return state_callerreferences(s, loc);
	}
	
	location_destroy(loc);
	return true;	
}


/* TODO: extract to own file */

struct vconst {
	struct map *varmap;
	struct map *keymap;
	struct map *persist;
};

struct vconst *
vconst_create()
{
	struct vconst *v = malloc(sizeof(struct vconst));
	v->varmap = map_create();
	v->keymap = map_create();
	v->persist = map_create();
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
	map_destroy(v->keymap);
	map_destroy(v->persist);
	free(v);
}

struct vconst *
vconst_copy(struct vconst *old)
{
	struct vconst *new = vconst_create();
	struct map *m = old->varmap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(
			new->varmap,
			dynamic_str(e.key),
			value_copy((struct value *) e.value)
		);
	}
	m = old->keymap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(
			new->keymap,
			dynamic_str(e.key),
			dynamic_str(e.value)
		);
	}
	m = old->persist;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(
			new->persist,
			dynamic_str(e.key),
			e.value
		);
	}

	return new;
}

static char *
vconst_id(struct map *varmap, struct map *persistmap, bool persist);

char *
vconst_declare(struct vconst *v, struct value *val, char *key, bool persist)
{
	struct map *m = v->varmap;

	char *s = vconst_id(m, v->persist, persist);

	map_set(m, dynamic_str(s), val);
	assert(key);
	map_set(v->keymap, dynamic_str(s), dynamic_str(key));
	map_set(v->persist, dynamic_str(s), (void *) persist);

	return s;
}

static int
count_true(struct map *m);

static char *
vconst_id(struct map *varmap, struct map *persistmap, bool persist)
{
	int npersist = count_true(persistmap);
	struct strbuilder *b = strbuilder_create();
	if (persist) {
		strbuilder_printf(b, "$%d", (int) npersist);
	} else {
		strbuilder_printf(b, "#%d", (int) varmap->n - npersist);
	}
	return strbuilder_build(b);
}

static int
count_true(struct map *m)
{
	int n = 0;
	for (int i = 0; i < m->n; i++) {
		if (m->entry[i].value) {
			n++;
		}
	}
	return n;
}

struct value *
vconst_get(struct vconst *v, char *id)
{
	return map_get(v->varmap, id);
}

struct value *
vconst_getbykey(struct vconst *v, char *key)
{
	/* XXX */
	struct map *m = v->keymap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		char *id = e.key,
		     *varkey = (char *) e.value;
		if (strcmp(key, varkey) == 0) {
			return vconst_get(v, id);
		}
	}
	return NULL;
}

void
vconst_undeclare(struct vconst *v)
{
	struct map *varmap = map_create(),
		   *keymap = map_create(),
		   *persist = map_create();

	struct map *m = v->varmap;
	for (int i = 0; i < m->n; i++) {
		char *key = m->entry[i].key;
		if (!map_get(v->persist, key)) {
			continue;
		}
		map_set(
			varmap, dynamic_str(key),
			value_copy((struct value *) map_get(v->varmap, key))
		);
		char *c = map_get(v->keymap, key);
		map_set(keymap, dynamic_str(key), dynamic_str(c));
		map_set(persist, dynamic_str(key), (void *) true);
	}

	v->varmap = varmap;
	v->keymap = keymap;
	v->persist = persist;
}

char *
vconst_str(struct vconst *v, char *indent)
{
	struct strbuilder *b = strbuilder_create();
	struct map *m = v->varmap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		char *value = value_str((struct value *) e.value);
		strbuilder_printf(b, "%s%s: %s", indent, e.key, value);
		char *key = map_get(v->keymap, e.key);
		strbuilder_printf(b, "\t\"%s\"", key);
		strbuilder_printf(b, "\n");
		free(value);
	}
	return strbuilder_build(b);
}

bool
vconst_eval(struct vconst *v, struct ast_expr *e)
{
	return ast_expr_matheval(e);
}
