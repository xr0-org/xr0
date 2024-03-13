#ifndef XR0_STATE_H
#define XR0_STATE_H

#include <stdbool.h>

#define KEYWORD_RESULT "result"

/* ast */
struct ast_type;
struct ast_variable;
struct ast_expr;

/* ext */
struct externals;

/* props */
struct props;

/* object */
struct object;

/* value */
struct value;

struct state;

struct state *
state_create(char *func, struct externals *, struct ast_type *result_type);

struct state *
state_create_withprops(char *func, struct externals *, struct ast_type *result_type,
		struct props *props, struct map *dedup);

struct state *
state_copy(struct state *);

struct state *
state_copywithname(struct state *, char *func_name);

void
state_destroy(struct state *state);

char *
state_str(struct state *);

struct externals *
state_getext(struct state *);

struct props *
state_getprops(struct state *);

struct heap *
state_getheap(struct state *);

struct map *
state_getdedup(struct state *);

void
state_pushframe(struct state *, char *func, struct ast_type *ret_type);

void
state_popframe(struct state *);

void
state_declare(struct state *, struct ast_variable *var, bool isparam);

struct object *
state_getresult(struct state *);

struct object *
state_getobject(struct state *, char *id);

struct ast_type *
state_getobjecttype(struct state *, char *id);

struct value *
state_getloc(struct state *state, char *id);

struct object *
state_deref(struct state *, struct value *ptr, struct ast_expr *index);

struct value *
state_alloc(struct state *);

struct error *
state_dealloc(struct state *, struct value *);

struct error *
state_range_alloc(struct state *, struct object *,
		struct ast_expr *lw, struct ast_expr *up);

struct error *
state_range_dealloc(struct state *, struct object *,
		struct ast_expr *lw, struct ast_expr *up);

bool
state_addresses_deallocand(struct state *, struct object *);

bool
state_range_aredeallocands(struct state *, struct object *,
		struct ast_expr *lw, struct ast_expr *up);

struct value *
state_vconst(struct state *, struct ast_type *, char *comment, bool persist);

struct value *
state_clump(struct state *);

bool
state_isclumped(struct state *, char *id);

bool
state_isalloced(struct state *, char *id);

struct value *
state_getvconst(struct state *, char *id);

bool
state_hasgarbage(struct state *);

bool
state_equal(struct state *s1, struct state *s2);


/* USED BY VALUE */

struct location;

struct location *
location_copy(struct location *);

void
location_destroy(struct location *);

char *
location_str(struct location *);

bool
location_references(struct location *l1, struct location *l2, struct state *);

bool
location_referencesheap(struct location *, struct state *);

struct value *
location_transfigure(struct location *, struct state *compare);

struct object *
state_get(struct state *state, struct location *loc, bool constructive);


/* USED BY OBJECT */

bool
state_isdeallocand(struct state *s, struct location *loc);

bool
state_eval(struct state *, struct ast_expr *);

#endif
