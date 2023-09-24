#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "ast.h"
#include "block.h"
#include "location.h"
#include "object.h"
#include "stack.h"
#include "util.h"

struct heap {
	struct block_arr *blocks;
	bool *freed; /* array of same length as blocks */
};

struct heap *
heap_create()
{
	struct heap *h = malloc(sizeof(struct heap));
	assert(h);
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

char *
heap_str(struct heap *h, char *indent)
{
	struct strbuilder *b = strbuilder_create();
	struct block_arr *arr = h->blocks;
	int n = block_arr_nblocks(arr);
	for (int i = 0; i < n; i++) {
		if (h->freed[i]) {
			continue;
		}
		char *block = block_str(block_arr_blocks(arr)[i]);
		strbuilder_printf(b, "%s%d: %s%s", indent, i, block,
				i+1 < n ? "\n" : "");
		free(block);
	}
	return strbuilder_build(b);
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
		
	return location_create(
		LOCATION_DYNAMIC, address, ast_expr_create_constant(0)
	);
}

block *
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
		return error_create("double free");
	}
	h->freed[address] = true;
	return NULL;
}

static bool
block_referenced(struct stack *s, struct heap *h, int addr);

bool
heap_referenced(struct heap *h, struct stack *s)
{
	int n = block_arr_nblocks(h->blocks);
	for (int i = 0; i < n; i++) {
		if (!h->freed[i] && !block_referenced(s, h, i)) {
			return false;	
		}
	}
	return true;
}

static bool
block_referenced(struct stack *s, struct heap *h, int addr)
{
	struct location *loc = location_create(
		LOCATION_DYNAMIC, addr, ast_expr_create_constant(0)
	);
	bool referenced = stack_references(s, h, loc); 
	location_destroy(loc);
	return referenced;
}
