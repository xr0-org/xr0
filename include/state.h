#ifndef XR0_STATE_H
#define XR0_STATE_H

#include <stdbool.h>

#define KEYWORD_RETURN "return"

/* ast */
struct ast_function;
struct ast_block;
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

enum execution_mode {
	EXEC_ABSTRACT,
	EXEC_ABSTRACT_NO_SETUP,
	EXEC_ACTUAL,
	EXEC_SETUP,
	EXEC_VERIFY
};

enum frame_kind {
	FRAME_NESTED,
	FRAME_INTERMEDIATE,
	FRAME_CALL,
	FRAME_SETUP
};

struct frame;

struct state *
state_create(struct frame *, struct externals *);

struct state *
state_create_withprops(struct frame *, struct externals *, struct props *props);

struct state *
state_copy(struct state *);

struct state *
state_copywithname(struct state *, char *func_name);

void
state_destroy(struct state *state);

char *
state_str(struct state *);

bool
state_atend(struct state *);

struct error *
state_step(struct state *);

struct error *
state_next(struct state *);

struct externals *
state_getext(struct state *);

struct props *
state_getprops(struct state *);

char *
state_programtext(struct state *);

int
state_programindex(struct state *);

int
state_frameid(struct state *);

struct heap *
state_getheap(struct state *);

void
state_pushframe(struct state *, struct frame *);

void
state_popframe(struct state *);

void
state_unnest(struct state *);

void
state_declare(struct state *, struct ast_variable *var, bool isparam);

struct object_res
state_getresult(struct state *);

struct object_res
state_getobject(struct state *, char *id);

struct ast_type *
state_getobjecttype(struct state *, char *id);

struct value *
state_getloc(struct state *state, char *id);

struct object_res
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
state_islinear(struct state *);

char *
state_execmode_str(enum execution_mode);

enum execution_mode
state_execmode(struct state *);

bool
state_addresses_deallocand(struct state *, struct object *);

bool
state_range_aredeallocands(struct state *, struct object *,
		struct ast_expr *lw, struct ast_expr *up);

struct value *
state_vconst(struct state *, struct ast_type *, char *comment, bool persist);

struct value *
state_static_init(struct state *, struct ast_expr *);

struct value *
state_clump(struct state *);

bool
state_islval(struct state *, struct value *);

bool
state_isalloc(struct state *, struct value *);

struct value *
state_getvconst(struct state *, char *id);

bool
state_hasgarbage(struct state *);

bool
state_equal(struct state *s1, struct state *s2);

struct value *
state_popregister(struct state *);

struct value *
state_readregister(struct state *);

void
state_writeregister(struct state *, struct value *);

void
state_clearregister(struct state *);

void
state_initsetup(struct state *s, int frameid);

enum execution_mode
state_next_execmode(struct state *s);

struct error *
state_stacktrace(struct state *, struct error *);

void
state_return(struct state *);

struct ast_expr *
state_framecall(struct state *);

struct location;

bool
state_returnreferences(struct state *, struct location *);

bool
state_callerreferences(struct state *, struct location *);

/* FRAME DTO */

struct frame *
frame_call_create(char *name, struct ast_block *, struct ast_type *,
		enum execution_mode, struct ast_expr *, struct ast_function *);

struct frame *
frame_block_create(char *name, struct ast_block *, enum execution_mode);

struct frame *
frame_setup_create(char *name, struct ast_block *, enum execution_mode);

struct frame *
frame_intermediate_create(char *name, struct ast_block *, enum execution_mode);

/* USED BY VALUE */

struct location *
location_copy(struct location *);

void
location_destroy(struct location *);

char *
location_str(struct location *);

bool
location_references(struct location *l1, struct location *l2, struct state *,
		struct circuitbreaker *cb);

bool
location_referencesheap(struct location *, struct state *,
		struct circuitbreaker *);

struct value *
location_transfigure(struct location *, struct state *compare);

struct int_arr *
location_deriveorder(struct location *, struct circuitbreaker *, struct state *);

struct permutation;

struct location *
location_permuteheap(struct location *loc, struct permutation *p);

struct object_res {
	struct object *obj;
	struct error *err;
};

struct object_res
state_get(struct state *state, struct location *loc, bool constructive);

/* USED BY OBJECT */

bool
state_isdeallocand(struct state *s, struct location *loc);

bool
state_eval(struct state *, struct ast_expr *);

struct block;

void
state_blockinstall(struct block *, struct object *);

struct block *
state_getblock(struct state *, struct location *);

#endif
