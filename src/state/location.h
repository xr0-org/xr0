#ifndef LOCATION_H
#define LOCATION_H

#include <stdbool.h>

struct ast_expr;

enum location_type {
	/* TODO: STATIC */
	LOCATION_VCONST, LOCATION_AUTOMATIC, LOCATION_DYNAMIC,
};

struct location;

struct location *
location_create(enum location_type type, int block, struct ast_expr *offset);

struct location *
location_copy(struct location *loc);

void
location_destroy(struct location *);

char *
location_str(struct location *);

bool
location_referencesheap(struct location *, struct state *);

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
location_references(struct location *loc1, struct location *loc2, struct state *);

struct vconst;

struct stack;

struct stack;
struct object;

struct block;

struct block *
location_getblock(struct location *, struct vconst *, struct stack *, struct heap *);

struct block *
location_getstackblock(struct location *loc, struct stack *s);

struct error *
location_dealloc(struct location *, struct heap *);

struct error *
location_range_dealloc(struct location *loc, struct ast_expr *lw,
		struct ast_expr *up, struct state *);

#endif
