#ifndef BLOCK_H
#define BLOCK_H

typedef struct object_arr block;

block *
block_create(int size);

void
block_destroy(block *);

char *
block_str(block *);

struct error;
struct value;
struct ast_expr;

struct stack;
struct heap;

struct object *
block_observe(block *, struct ast_expr *offset, struct heap *);

struct error *
block_range_alloc(block *b, struct ast_expr *lw, struct ast_expr *up,
		struct heap *heap);

bool
block_range_aredeallocands(block *, struct ast_expr *lw, struct ast_expr *up,
		struct heap *);

struct error *
block_range_dealloc(block *, struct ast_expr *lw, struct ast_expr *up,
		struct heap *);


struct block_arr;

struct block_arr *
block_arr_create();

void
block_arr_destroy(struct block_arr *);

block **
block_arr_blocks(struct block_arr *);

int
block_arr_nblocks(struct block_arr *);

/* block_arr_append: Append block to array and return index (address). */
int
block_arr_append(struct block_arr *, block *);

void
block_arr_delete(struct block_arr *, int address);

#endif
