#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "ast.h"
#include "location.h"
#include "block.h"
#include "util.h"

struct clump {
	struct block_arr *blocks;	
};

struct clump *
clump_create()
{
	struct clump *c = malloc(sizeof(struct clump));
	assert(c);
	c->blocks = block_arr_create();
	return c;
}

void
clump_destroy(struct clump *c)
{
	block_arr_destroy(c->blocks);
}

char *
clump_str(struct clump *c, char *indent)
{
	struct strbuilder *b = strbuilder_create();
	int n = block_arr_nblocks(c->blocks);
	struct block **arr = block_arr_blocks(c->blocks);
	for (int i = 0; i < n; i++) {
		char *block = block_str(arr[i]);
		strbuilder_printf(b, "%s%d: %s\n", indent, i, block);
		free(block);
	}
	return strbuilder_build(b);
}

struct clump *
clump_copy(struct clump *c)
{
	struct clump *copy = malloc(sizeof(struct clump));	
	copy->blocks = block_arr_copy(c->blocks);
	return copy;
}

int
clump_newblock(struct clump *c)
{
	int address = block_arr_append(c->blocks, block_create());

	int n = block_arr_nblocks(c->blocks);
	assert(n > 0);
		
	return address;	
}

struct block *
clump_getblock(struct clump *c, int address)
{
	if (address >= block_arr_nblocks(c->blocks)) {
		return NULL;
	}
	return block_arr_blocks(c->blocks)[address];
}
