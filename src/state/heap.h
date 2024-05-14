#ifndef HEAP_H
#define HEAP_H

#include "block.h"

struct heap;

struct heap *
heap_create();

void
heap_destroy();

struct heap *
heap_copy(struct heap *);

char *
heap_str(struct heap *, char *indent);

struct permutation;

struct permutation *
heap_transposezero(struct heap *, int block);

struct heap *
heap_permute(struct heap *, struct permutation *p);

struct location_arr;

struct ast_expr;

struct block_arr *
heap_blocks(struct heap *);

struct location *
heap_newblock(struct heap *);

struct location *
heap_newcallerblock(struct heap *);

struct block; 

struct block *
heap_getblock(struct heap *h, int block);

bool
heap_referenced(struct heap *h, struct state *);

bool
heap_blockisfreed(struct heap *h, int block);

struct error *
heap_deallocblock(struct heap *h, int block);

void
heap_undeclare(struct heap *, struct state *);


/* TODO: extract to own file */

struct vconst;

struct vconst *
vconst_create();

struct vconst *
vconst_copy(struct vconst *);

void
vconst_destroy(struct vconst *);

char *
vconst_str(struct vconst *, char *indent);

char *
vconst_declare(struct vconst *, struct value *, char *comment, bool persist);

struct value *
vconst_get(struct vconst *, char *id);

void
vconst_undeclare(struct vconst *);

bool
vconst_eval(struct vconst *, struct ast_expr *);

#endif
