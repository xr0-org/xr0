#ifndef HEAP_H
#define HEAP_H

#include "block.h"

struct heap;

struct heap *
heap_create();

void
heap_destroy();

char *
heap_str(struct heap *, char *indent);

struct ast_expr;

struct block_arr *
heap_blocks(struct heap *);

struct location *
heap_newblock(struct heap *h, int size);

block *
heap_getblock(struct heap *h, int block);

bool
heap_referenced(struct heap *h, struct stack *);

struct error *
heap_deallocblock(struct heap *h, int block);

#endif
