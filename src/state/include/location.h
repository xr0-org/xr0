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

struct offset;
struct location;

struct location *
location_create_static(int block, struct offset *offset);

struct location *
location_create_rconst(int block, struct offset *offset);

struct location *
location_create_dereferencable(int block, struct offset *offset);

struct location *
location_create_dynamic(int block, struct offset *offset);

struct location *
location_create_automatic(int frame, int block, struct offset *offset);

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

struct offset *
location_offset(struct location *loc);

void
location_setoffset(struct location *loc, struct offset *offset);

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

struct rconst;

struct clump;

struct stack;

struct object;

struct block;

struct block_res *
location_getblock(struct location *, struct static_memory *, struct rconst *, struct stack *,
		struct heap *, struct clump *);

struct block *
location_getstackblock(struct location *loc, struct stack *s);

struct error *
location_dealloc(struct location *, struct heap *);

struct location_arr;

struct location_arr *
location_arr_create(void);

void
location_arr_destroy(struct location_arr *);

struct location **
location_arr_loc(struct location_arr *);

int
location_arr_n(struct location_arr *);

void
location_arr_append(struct location_arr *, struct location *);


struct offset;
struct ast_type;

struct offset *
offset_create(struct ast_expr *o);

struct offset *
offset_create_member(struct offset *o, char *member, struct ast_type *membertype);

struct offset *
offset_copy(struct offset *);

void
offset_destroy(struct offset *);

char *
offset_str(struct offset *);

struct ast_expr *
offset_as_expr(struct offset *);

struct ast_expr *
offset_offset(struct offset *);

char *
offset_member(struct offset *);

struct ast_type *
offset_membertype(struct offset *);

#endif
