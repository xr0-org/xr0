#ifndef XR0_STATE_H
#define XR0_STATE_H

/* util.h */
struct map;

struct state;

struct state *
state_create(struct map *extfunc, struct ast_type *result_type);

void
state_destroy(struct state *state);

bool
state_abstractly_equivalent(struct state *s1, struct state *s2);

bool
state_heap_referenced(struct state *state);

void
state_pushframe(struct state *state, struct ast_type *type);

void
state_popframe(struct state *state);

void
state_declare(struct state *state, struct ast_variable *var, bool isparam);

struct ast_variable **
state_getvariables(struct state *state);

int
state_nvariables(struct state *state);

struct map *
state_extfunc(struct state *state);

struct ast_function *
state_getfunc(struct state *state, char *f);

struct reference *
state_getresult(struct state *state);

typedef struct reference Ref;

Ref *
ref_create_offset(char *id, int nderef, struct ast_expr *offset);

Ref *
ref_create_range(char *id, int nderef, struct ast_expr *lower,
		struct ast_expr *upper);

void
ref_destroy(Ref *ref);

bool
ref_isresult(Ref *ref);

char *
ref_id(Ref *ref);

int
ref_nderef(Ref *ref);

char *
ref_str(Ref *ref);

bool
state_ref_onheap(struct state *state, Ref *ref);

struct error *
state_ref_unalloc(struct state *state, Ref *ref);

typedef struct heaploc Heaploc;

struct error *
state_heaploc_free(struct state *state, Heaploc *loc);

Heaploc *
state_ref_get_heaploc(struct state *state, Ref *ref);

void
state_ref_assign_heaploc(struct state *state, Ref *ref, Heaploc *loc);

bool
state_ref_canfree(struct state *state, Ref *ref);

void
state_result_assign(struct state *state, Heaploc *loc);

Heaploc *
state_alloc(struct state *state);

char *
state_str(struct state *state);

#endif
