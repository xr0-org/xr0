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

/* object */
struct object;

/* value */
struct value;

struct state;

struct frame;

struct rconst;

struct state *
state_create(struct frame *, struct rconst *, struct externals *);

struct number;

struct state *
state_copy(struct state *);

struct state *
state_split(struct state *, struct rconst *, char *funcname);

char *
state_funcname(struct state *s);

void
state_destroy(struct state *state);

char *
state_str(struct state *);

int
state_atend(struct state *);

int
state_atsetupend(struct state *);

int
state_atinvariantend(struct state *);

int
state_atloopend(struct state *);

struct error *
state_step(struct state *);

struct error *
state_next(struct state *);

struct externals *
state_getext(struct state *);

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

struct ast_type *
state_getreturntype(struct state *);

bool
state_isparam(struct state *, char *id);

struct object_res *
state_getobject(struct state *, char *id);

struct ast_type *
state_getvariabletype(struct state *, char *id);

struct location;
struct circuitbreaker;

DECLARE_RESULT_TYPE(struct location *, loc, loc_res)

struct loc_res *
state_getloc(struct state *state, char *id);

struct object_res *
state_deref(struct state *, struct value *ptr);

struct location;

struct location *
state_alloc(struct state *, int size);

struct error *
state_dealloc(struct state *, struct location *);

bool
state_islinear(struct state *);

int
state_modecanverify(struct state *);

int
state_modecanrunxr0cmd(struct state *);


struct stack;

struct stack *
state_stack(struct state *);

struct lexememarker *
state_lexememarker(struct state *);

bool
state_addresses_deallocand(struct state *, struct object *);

struct value *
state_rconst(struct state *, struct ast_type *, struct ast_expr *range,
		char *key, bool persist);

struct value *
state_rconstnokey(struct state *, struct ast_type *, struct ast_expr *range,
		bool persist);

struct value *
state_static_init(struct state *, struct ast_expr *);

struct value *
state_clump(struct state *, int size);

bool
state_loc_valid(struct state *, struct location *);

bool
state_loc_onheap(struct state *, struct location *);

struct value *
state_getrconst(struct state *, char *id);

bool
state_hasgarbage(struct state *);

struct error *
state_specverify(struct state *actual, struct state *spec);

struct value *
state_popregister(struct state *);

struct value *
state_readregister(struct state *);

void
state_writeregister(struct state *, struct value *);

void
state_clearregister(struct state *);

struct error *
state_stacktrace(struct state *, struct error *);

void
state_return(struct state *);

void
state_break(struct state *);

struct ast_expr *
state_framecall(struct state *);

/* state_argmodulator: return a string that is unique down to the values of the
 * arguments for the call in the nearest call frame. return an empty string if
 * the nearest call frame is the base frame. */
char *
state_argmodulator(struct state *);

bool
state_returnreferences(struct state *, struct location *);

bool
state_callerreferences(struct state *, struct location *);

/* FRAME DTO */

struct frame *
frame_callabstract_create(char *name, struct ast_block *, struct ast_expr *,
		struct ast_function *);

struct frame *
frame_callactual_create(char *name, struct ast_block *, struct ast_expr *,
		struct ast_function *);

struct frame *
frame_blockverify_create(char *name, struct ast_block *);

void
state_pushinvariantframe(struct state *, struct ast_block *);

void
state_pushloopframe(struct state *, struct ast_block *);

struct frame *
frame_blockfindsetup_create(char *name, struct ast_block *);

struct frame *
frame_blocksame_create(char *name, struct ast_block *, struct state *);

struct frame *
frame_setup_create(char *name, struct ast_block *);

struct frame *
frame_linear_create(char *name, struct ast_block *, struct state *);

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

struct location *
location_withoutoffset(struct location *);

struct offset;

struct offset *
offset_create(struct ast_expr *offset);

struct offset *
offset_create_member(struct offset *o, char *member, struct ast_type *membertype);

struct ast_expr *
offset_as_expr(struct offset *);

struct offset *
location_offset(struct location *loc);

void
location_setoffset(struct location *loc, struct offset *offset);

struct int_arr *
location_deriveorder(struct location *, struct circuitbreaker *, struct state *);

struct permutation;

struct location *
location_permuteheap(struct location *loc, struct permutation *p);

struct object_res *
state_get(struct state *state, struct location *loc, bool constructive);

struct error *
state_constraintverify(struct state *spec, struct state *impl, char *id);

struct error *
state_constraintverify_structmember(struct state *spec, struct state *impl,
		struct value *spec_v, struct value *impl_v, char *member);

struct error *
state_constraintverify_all(struct state *spec, struct state *impl);

struct error *
state_verifyinvariant(struct state *);

/* USED BY OBJECT */

bool
state_isdeallocand(struct state *s, struct location *loc);

bool
state_eval(struct state *, struct ast_expr *);

struct block;

struct block *
state_getblock(struct state *, struct location *);

#endif
