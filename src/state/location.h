#ifndef LOCATION_H
#define LOCATION_H

#include <stdbool.h>

struct ast_expr;

enum location_type {
	LOCATION_STATIC,
	LOCATION_VCONST,
	LOCATION_DEREFERENCABLE,
	LOCATION_AUTOMATIC,
	LOCATION_DYNAMIC,
};

struct location;

struct location *
location_create_static(int block, struct ast_expr *offset);

struct location *
location_create_vconst(int block, struct ast_expr *offset);

struct location *
location_create_dereferencable(int block, struct ast_expr *offset);

struct location *
location_create_dynamic(int block, struct ast_expr *offset);

struct location *
location_create_automatic(int frame, int block, struct ast_expr *offset);

struct value *
location_transfigure(struct location *, struct state *compare);

void
location_setframe(struct location *loc, int frame);

int
location_getframe(struct location *loc);

struct location *
location_copy(struct location *loc);

void
location_destroy(struct location *);

char *
location_str(struct location *);

bool
location_isauto(struct location *);

struct static_memory;

bool
location_tostatic(struct location *, struct static_memory *);

struct heap;

bool
location_toheap(struct location *, struct heap *);

struct stack;

bool
location_tostack(struct location *, struct stack *);

struct clump;

bool
location_toclump(struct location *, struct clump *);

bool
location_referencesheap(struct location *, struct state *, struct circuitbreaker *);

enum location_type
location_type(struct location *loc);

struct ast_expr *
location_offset(struct location *loc);

struct location *
location_with_offset(struct location *loc, struct ast_expr *offset);

struct heap;

struct state;

bool
location_equal(struct location *loc1, struct location *loc2);

bool
location_referencescaller(struct location *l1, struct location *l2, struct state *s,
		struct circuitbreaker *cb);

bool
location_references(struct location *loc1, struct location *loc2, struct state *,
		struct circuitbreaker *cb);

struct permutation;

struct permutation *
location_heaptransposezero(struct location *, struct heap *); 

struct static_memory;

struct vconst;

struct clump;

struct stack;

struct object;

struct block;

struct block_res {
	struct block *b;
	struct error *err;
};

struct block_res
location_getblock(struct location *, struct static_memory *, struct vconst *, struct stack *,
		struct heap *, struct clump *);

struct block *
location_getstackblock(struct location *loc, struct stack *s);

struct error *
location_dealloc(struct location *, struct heap *);

struct error *
location_range_dealloc(struct location *loc, struct ast_expr *lw,
		struct ast_expr *up, struct state *);

struct location_arr;

struct location_arr *
location_arr_create();

void
location_arr_destroy();

struct location **
location_arr_loc(struct location_arr *);

int
location_arr_n(struct location_arr *);

void
location_arr_append(struct location_arr *, struct location *);

#endif
