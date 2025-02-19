#ifndef XR0_VALUE_H
#define XR0_VALUE_H

#include <stdbool.h>

struct value;

struct location;

struct permutation;

struct int_arr;

struct ast_type;

struct state;

struct int_arr *
value_deriveorder(struct value *, struct circuitbreaker *, struct state *);

struct value *
value_permuteheaplocs(struct value *, struct permutation *);

struct ast_type;

struct value *
value_ptr_create(struct location *loc);

struct value *
value_ptr_rconst_create(void);

struct value *
value_int_create(int val);

int
value_isint(struct value *v);

struct value *
value_literal_create(char *);

struct ast_expr;

struct value *
value_int_rconst_create(struct ast_expr *range, struct state *);

struct value *
value_int_ne_create(int not_val);

struct value *
value_int_range_create(int lw, int excl_up);

int
value_int_lw(struct value *, struct state *);

int
value_int_up(struct value *, struct state *);

int
value_as_int(struct value *, struct state *);

struct value *
value_bang(struct value *);

struct value *
value_rconst_create(struct ast_expr *);

struct value *
value_struct_create(struct ast_type *);

bool
value_isstruct(struct value *v);

struct value *
value_struct_rconst_create(struct ast_type *, struct state *,
		char *key, bool persist);

struct value *
value_struct_rconstnokey_create(struct ast_type *, struct state *, bool persist);

struct value *
value_pf_augment(struct value *, struct ast_expr *root);

struct ast_type *
value_struct_membertype(struct value *, char *member);

struct object *
value_struct_member(struct value *, char *member);

struct error *
value_struct_specval_verify(struct value *param, struct value *arg,
		struct state *spec, struct state *caller);

struct value *
value_copy(struct value *);

struct value *
value_abstractcopy(struct value *, struct state *s);

void
value_destroy(struct value *);

char *
value_str(struct value *);

char *
value_type_str(struct value *);

bool
value_islocation(struct value *);

struct location *
value_as_location(struct value *);

struct circuitbreaker;

bool
value_referencesheap(struct value *, struct state *, struct circuitbreaker *);

int
value_isconstant(struct value *v);

int
value_as_constant(struct value *v);

bool
value_issync(struct value *v);

struct ast_expr *
value_as_rconst(struct value *v);

struct ast_expr *
value_to_expr(struct value *);

bool
value_isliteral(struct value *v);

struct ast_expr *
value_as_literal(struct value *v);

bool
value_references(struct value *, struct location *, struct state *,
		struct circuitbreaker *);

DECLARE_RESULT_TYPE(bool, bool, bool_res)

int
value_eq(struct value *lhs, struct value *rhs, struct state *);

int
value_lt(struct value *lhs, struct value *rhs, struct state *);

struct error *
value_disentangle(struct value *, struct value *, struct state *);

/* value_confirmsubset: returns an error if v (as belonging to s) is not
 * decidably a subset of v0 (as belonging to s0). */
struct error *
value_confirmsubset(struct value *v, struct value *v0, struct state *s,
		struct state *s0);

struct number;

/* value_splitassume: returns false if contradiction encountered. */
bool
value_splitassume(struct value *, struct number *);

struct splitinstruct;

struct splitinstruct *
splitinstruct_create(void);

void
splitinstruct_append(struct splitinstruct *, struct map *);

int 
splitinstruct_n(struct splitinstruct *);

struct map **
splitinstruct_splits(struct splitinstruct *);


DECLARE_RESULT_TYPE(struct value *, value, value_res)

struct value_arr;

struct value_arr *
value_arr_create(void);

void
value_arr_destroy(struct value_arr *arr);

void
value_arr_append(struct value_arr *arr, struct value *res);

int
value_arr_len(struct value_arr *arr);

struct value **
value_arr_v(struct value_arr *arr);

DECLARE_RESULT_TYPE(struct value_arr *, arr, value_arr_res)

struct number;

struct cconst;


struct number *
number_cconst_create(struct cconst *);

/* TODO: remove */
struct number *
number_single_create(int);

struct number *
number_singlerange_create(struct number *lw, struct number *up);

struct number *
number_copy(struct number *);

void
number_destroy(struct number *);

char *
number_str(struct number *);

char *
number_str_inrange(struct number *);

int
number_iscconst(struct number *);

struct cconst *
number_as_cconst(struct number *);

int
number_isrange(struct number *);

struct range;

struct range *
number_as_range(struct number *);

int
number_isexpr(struct number *);

struct ast_expr *
number_as_expr(struct number *);

int
number_isconstant(struct number *);

int
number_eq(struct number *, struct number *);

int
number_le(struct number *lhs, struct number *rhs);

int
number_lt(struct number *lhs, struct number *rhs);

int
number_ge(struct number *lhs, struct number *rhs);

int
numbers_aresinglerange(struct number *lw, struct number *up);

struct number *
number_lw(struct number *, struct state *);

struct number *
number_up(struct number *, struct state *);

#endif
