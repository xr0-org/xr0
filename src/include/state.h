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
state_create(struct frame *, struct externals *);

struct number;

struct state *
state_copy(struct state *);

struct lsi_le;

struct state *
state_split(struct state *, struct lsi_le *);

void
state_destroy(struct state *state);

char *
state_str(struct state *);

int
state_atend(struct state *);

int
state_atsetupend(struct state *);

int
state_ininvariant(struct state *);

int
state_atinvariantend(struct state *);

int
state_atloopend(struct state *);

struct error *
state_step(struct state *);

struct error *
state_next(struct state *);

void
state_endinvariant(struct state *);

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

/* state_goto: return 0 if unable, nonzero otherwise. */
int
state_goto(struct state *, char *);

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
state_alloc(struct state *, struct value *size);

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

char *
state_baseframename(struct state *);

struct rconst *
state_rconstxxx(struct state *);

char *
state_rconst(struct state *, char *key, bool persist);

char *
state_rconstnokey(struct state *, bool persist);

int
state_rconst_isanyint(struct state *, char *rconst);

struct str_res;

struct str_res *
state_getrconstwithvalue(struct state *, int);

struct lsi_le;

struct error *
state_addconstraint(struct state *, struct lsi_le *);

int
state_satisfies(struct state *, struct lsi_le *);

struct lsi_range;
struct lsi_expr;

struct lsi_range *
state_range_eval(struct state *, struct lsi_expr *);

struct value *
state_static_init(struct state *, struct ast_expr *);

struct value *
state_clump(struct state *, struct value *size);

bool
state_loc_valid(struct state *, struct location *);

bool
state_loc_onheap(struct state *, struct location *);

int
state_hasrconst(struct state *state, char *id);

struct value *
state_getrconst(struct state *, char *id);

bool
state_hasgarbage(struct state *);

struct error *
state_verify_endstate(struct state *impl, struct state *spec);

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
state_get(struct state *state, struct location *loc);

struct error *
state_verify_callsetup(struct state *spec, struct state *impl);

struct error *
state_constraintverify_all(struct state *spec, struct state *impl);

struct error *
state_shapeverify_structmember(struct state *spec, struct state *impl,
		struct value *spec_v, struct value *impl_v, char *member);

struct lsi_varmap;

struct lsi_varmap *
state_impl_spec_mapping_structmember(struct state *spec, struct state *impl,
		struct value *spec_v, struct value *impl_v, char *parent,
		char *member);

struct lsi_varmap *
state_rconst_mapping_structmember(struct state *, struct value *, char *parent,
		char *member);

struct lsi_varmap *
state_block_rconst_mapping(struct state *, struct location *, struct ast_type *,
		char *referent);

struct error *
state_verify_invariant(struct state *);

struct lsi_le;

int
state_isfeasible(struct state *, struct lsi_le *);

/* USED BY OBJECT */

bool
state_isdeallocand(struct state *s, struct location *loc);

struct block;

struct block *
state_getblock(struct state *, struct location *);


/* state_arr */

struct state_arr *
state_arr_create(void);

struct state_arr *
state_arr_copy(struct state_arr *);

void
state_arr_destroy(struct state_arr *);

void
state_arr_append(struct state_arr *, struct state *);

int
state_arr_len(struct state_arr *);

struct state **
state_arr_s(struct state_arr *);

#endif
