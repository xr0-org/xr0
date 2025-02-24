#ifndef XR0_VALUE_NUMBER_H
#define XR0_VALUE_NUMBER_H

struct number;

struct number *
number_const_create(long);

struct ast_expr;

struct number *
number_expr_create(struct ast_expr *);

struct range;

struct number *
number_range_create(struct range *);

struct number *
number_ne_create(long not_val);

struct number *
number_copy(struct number *);

void
number_destroy(struct number *);

char *
number_str(struct number *);

char *
number_short_str(struct number *);

struct range;

int
number_isexpr(struct number *);

struct ast_expr *
number_as_expr(struct number *);

struct ast_expr *
number_to_expr(struct number *);

long
number_as_const(struct number *);

int
number_isconst(struct number *);

struct range *
number_as_range(struct number *);

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
