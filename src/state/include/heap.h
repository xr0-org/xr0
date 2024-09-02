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
heap_newblock(struct heap *, int size);

struct location *
heap_newcallerblock(struct heap *, int size);

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

void
heap_fillorder(struct heap *, struct int_arr *);

/* TODO: extract to own file */

struct rconst;

struct rconst *
rconst_create();

struct rconst *
rconst_copy(struct rconst *);

void
rconst_destroy(struct rconst *);

char *
rconst_str(struct rconst *, char *indent);

char *
rconst_declare(struct rconst *, struct value *, char *key, bool persist);

char *
rconst_declarenokey(struct rconst *, struct value *, bool persist);

struct value *
rconst_get(struct rconst *, char *id);

char *
rconst_getidbykey(struct rconst *, char *key);

void
rconst_undeclare(struct rconst *);

bool
rconst_eval(struct rconst *, struct ast_expr *);

#endif
