#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "ast.h"
#include "location.h"
#include "block.h"
#include "value.h"
#include "util.h"

struct static_memory {
	struct block_arr *blocks;	
	struct map *pool;
};

struct static_memory *
static_memory_create(void)
{
	struct static_memory *sm = malloc(sizeof(struct static_memory));
	assert(sm);
	sm->blocks = block_arr_create();
	sm->pool = map_create(); 
	return sm;
}

void
static_memory_destroy(struct static_memory *sm)
{
	block_arr_destroy(sm->blocks);
}

char *
static_memory_str(struct static_memory *sm, char *indent)
{
	struct strbuilder *b = strbuilder_create();
	int n = block_arr_nblocks(sm->blocks);
	struct block **arr = block_arr_blocks(sm->blocks);
	for (int i = 0; i < n; i++) {
		char *block = block_str(arr[i]);
		strbuilder_printf(b, "%s%d: %s\n", indent, i, block);
		free(block);
	}
	return strbuilder_build(b);
}

static struct map *
pool_copy(struct map *);

struct static_memory *
static_memory_copy(struct static_memory *sm)
{
	struct static_memory *copy = malloc(sizeof(struct static_memory));	
	copy->blocks = block_arr_copy(sm->blocks);
	copy->pool = pool_copy(sm->pool);
	return copy;
}

static struct map *
pool_copy(struct map *p)
{
	struct map *pcopy = map_create();
	for (int i = 0; i < p->n; i++) {
		struct entry e = p->entry[i];
		map_set(
			pcopy,
			dynamic_str(e.key),
			location_copy((struct location *) e.value)
		);
	}
	return pcopy;
}

int
static_memory_newblock(struct static_memory *sm)
{
	int address = block_arr_append(sm->blocks, block_create(1));

	int n = block_arr_nblocks(sm->blocks);
	assert(n > 0);
	return address;
}

struct block *
static_memory_getblock(struct static_memory *sm, int address)
{
	if (address >= block_arr_nblocks(sm->blocks)) {
		return NULL;
	}
	return block_arr_blocks(sm->blocks)[address];
}

/* string pooling to write one string literal to static_memory and all subsequent
 * literals reference it is used to avoid infinite loops in splitting logic with
 * different literals per selection condition */
void
static_memory_stringpool(struct static_memory *sm, char *lit, struct location *loc)
{
	map_set(
		sm->pool,
		dynamic_str(lit),
		location_copy(loc)
	);
}

struct location *
static_memory_checkpool(struct static_memory *sm, char *lit)
{
	return (struct location *) map_get(sm->pool, lit);
}
