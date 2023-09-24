#ifndef XR0_STATE_H
#define XR0_STATE_H

#include <stdbool.h>

#define KEYWORD_RESULT "result"

/* util.h */
struct map;

struct state;
struct ast_type;

struct state *
state_create(struct map *extfunc, struct ast_type *result_type);

void
state_destroy(struct state *state);

char *
state_str(struct state *state);

struct map *
state_extfunc(struct state *state);

struct ast_function *
state_getfunc(struct state *state, char *f);

void
state_pushframe(struct state *state, struct ast_type *ret_type);

void
state_popframe(struct state *state);

struct ast_variable;

void
state_declare(struct state *state, struct ast_variable *var, bool isparam);

struct ast_variable **
state_getvariables(struct state *state);

int
state_nvariables(struct state *state);

struct variable;
struct location;

struct location *
state_getresultloc(struct state *state);

struct ast_expr;

struct location *
state_location_from_lvalue(struct state *state, struct ast_expr *lvalue);

struct location *
state_location_from_rvalue(struct state *state, struct ast_expr *rvalue);

/* state_location_assign: Assign the value in the object denoted by r to
 * the object denoted by l. */
struct error *
state_location_assign(struct state *state, struct location *l, struct location *r);

/* state_location_alloc_range: Assign a (virtual) array to the object denoted by
 * loc on the range [lower, upper). */
struct error *
state_location_range_alloc(struct state *state, struct location *loc,
		struct ast_expr *lw, struct ast_expr *up);

struct error *
state_location_dealloc(struct state *state, struct location *loc);

struct error *
state_location_range_dealloc(struct state *state, struct location *loc,
		struct ast_expr *lower, struct ast_expr *upper);

bool
state_location_addresses_deallocand(struct state *state, struct location *loc);

bool
state_location_range_aredeallocands(struct state *state, struct location *loc,
		struct ast_expr *lower, struct ast_expr *upper);

struct location *
state_alloc(struct state *state, int size);

bool
state_abstractly_equivalent(struct state *s1, struct state *s2);

bool
state_heap_referenced(struct state *state);

void
state_location_destroy(struct location *loc);

#endif
