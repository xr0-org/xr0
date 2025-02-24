#ifndef XR0_VALUE_NUMBER_H
#define XR0_VALUE_NUMBER_H

struct number;

struct cconst;

struct number *
number_cconst_create(struct cconst *);

struct number *
number_expr_create(struct ast_expr *);

/* TODO: remove */
struct number *
number_single_create(int);

struct range;

struct range_arr;

struct number *
number_ranges_create(struct range_arr *ranges);

struct number *
number_copy(struct number *);

void
number_destroy(struct number *);

char *
number_str(struct number *);

char *
number_str_inrange(struct number *);

int
number_isrange(struct number *);

struct range;

int
number_isexpr(struct number *);

struct ast_expr *
number_as_expr(struct number *);

struct ast_expr *
number_to_expr(struct number *);

struct cconst *
number_as_cconst(struct number *);

int
number_isconstant(struct number *);

int
number_lt(struct number *lhs, struct number *rhs, struct state *);

int
number_le(struct number *lhs, struct number *rhs, struct state *);

int
number_eq(struct number *, struct number *, struct state *);

int
number_ge(struct number *lhs, struct number *rhs, struct state *);

int
number_issinglerange(struct number *, struct state *);

int
numbers_aresinglerange(struct number *lw, struct number *up);

struct number *
number_tosinglerange(struct number *, struct state *);

struct state;

struct number *
number_lw(struct number *, struct state *);

struct number *
number_up(struct number *, struct state *);

struct map;

void
number_splitto(struct number *n, struct number *range, struct map *splits,
		struct state *s);

int
number_assume(struct number *n, struct number *split, struct state *);

struct error *
number_disentangle(struct number *, struct number *, struct state *);

#endif
