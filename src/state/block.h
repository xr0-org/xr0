#ifndef BLOCK_H
#define BLOCK_H

struct block;

struct block *
block_create();

void
block_destroy(struct block *);

char *
block_str(struct block *);

struct error;
struct value;
struct ast_expr;

struct stack;
struct heap;
struct object;

void
block_install(struct block *, struct object *);

struct state;

struct object *
block_observe(struct block *, struct ast_expr *offset, struct state *,
		bool constructive);

struct location;

struct circuitbreaker;

bool
block_references(struct block *, struct location *, struct state *,
		struct circuitbreaker *);

struct error *
block_range_alloc(struct block *b, struct ast_expr *lw, struct ast_expr *up,
		struct heap *heap);

struct state;

bool
block_range_aredeallocands(struct block *, struct ast_expr *lw, struct ast_expr *up,
		struct state *);

struct error *
block_range_dealloc(struct block *, struct ast_expr *lw, struct ast_expr *up,
		struct state *);

void
block_undeclare(struct block *, struct state *);

struct block_arr;

struct block_arr *
block_arr_create();

void
block_arr_destroy(struct block_arr *);

struct block_arr *
block_arr_copy(struct block_arr *);

struct block **
block_arr_blocks(struct block_arr *);

int
block_arr_nblocks(struct block_arr *);

/* block_arr_append: Append struct block to array and return index (address). */
int
block_arr_append(struct block_arr *, struct block *);

void
block_arr_delete(struct block_arr *, int address);

#endif
