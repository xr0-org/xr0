#ifndef XR0_STATE_H
#define XR0_STATE_H

#include <stdbool.h>

struct externals;

struct externals *
externals_create();

void
externals_destroy(struct externals *);

struct ast_function;
struct ast_variable;
struct ast_type;

void
externals_declarefunc(struct externals *, char *id, struct ast_function *);

void
externals_declarevar(struct externals *, char *id, struct ast_variable *);

void
externals_declaretype(struct externals *, char *id, struct ast_type *type);

struct ast_function *
externals_getfunc(struct externals *, char *id);

struct ast_type *
externals_gettype(struct externals *, char *id);


#define KEYWORD_RESULT "result"

/* util.h */

struct state;
struct ast_type;

struct state *
state_create(struct externals *, struct ast_type *result_type);

struct state *
state_copy(struct state *);

void
state_destroy(struct state *state);

char *
state_str(struct state *);

struct ast_function *
state_getfunc(struct state *, char *f);

void
state_pushframe(struct state *, struct ast_type *ret_type);

void
state_popframe(struct state *);

struct ast_variable;

void
state_declare(struct state *, struct ast_variable *var, bool isparam);

void
state_stack_undeclare(struct state *s);

struct location;

struct object *
state_get(struct state *state, struct location *loc, bool constructive);

struct block *
state_getblock(struct state *state, struct location *loc);

struct variable;
struct object;

struct object *
state_getresult(struct state *);

struct ast_expr;

struct ast_type *
state_gettype(struct state *, char *id);

struct object *
state_getobject(struct state *, char *id);

struct object *
state_getobjectmember(struct state *, struct object *, struct ast_type *,
		char *member);

struct ast_type *
state_getobjectmembertype(struct state *state, struct object *obj,
		struct ast_type *t, char *member);

struct object *
state_deref(struct state *, struct object *ptr, struct ast_expr *index);

struct value *
state_getvalue(struct state *, struct ast_expr *rvalue);

struct error *
state_assign(struct state *, struct object *, struct value *);

struct value *
state_boundedincrement(struct state *, char *id);

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
state_isdeallocand(struct state *s, struct location *loc);

bool
state_addresses_deallocand(struct state *, struct object *);

bool
state_range_aredeallocands(struct state *, struct object *,
		struct ast_expr *lw, struct ast_expr *up);

bool
state_abstractly_equivalent(struct state *s1, struct state *s2, bool paramonly);

bool
state_heap_referenced(struct state *);

void
state_object_destroy(struct object *);

void
state_value_destroy(struct value *);

struct value *
state_value_copy(struct value *);

struct value *
object_as_value(struct object *obj);

struct value *
state_getvconst(struct state *state, char *id);

struct value *
value_int_create(int val);

struct value *
value_int_range_create(int lw, int excl_up);

struct value *
value_int_sync_create(struct ast_expr *);

struct ast_expr *
value_as_sync(struct value *v);

struct object *
value_struct_member(struct value *, char *member);

struct value *
value_literal_create(char *);

struct ast_expr *
value_to_expr(struct value *);

char *
value_str(struct value *);

enum ast_binary_operator;

bool
value_compare(struct value *v1, enum ast_binary_operator, struct value *v2);

struct value *
state_vconst(struct state *state);


bool
state_stack_references(struct state *s, struct location *loc);

bool
state_eval(struct state *, struct ast_expr *);

bool
state_equal(struct state *s1, struct state *s2);

#endif
