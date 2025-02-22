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

int
number_isexpr(struct number *);

struct ast_expr *
number_as_expr(struct number *);

struct ast_expr *
number_to_expr(struct number *);

int
number_isconstant(struct number *);

int
number_lt(struct number *lhs, struct number *rhs, struct state *s_lhs,
		struct state *s_rhs);

int
number_le(struct number *lhs, struct number *rhs, struct state *s_lhs,
		struct state *s_rhs);

int
number_eq(struct number *n, struct number *n0, struct state *s_n,
		struct state *s_n0);

int
number_ge(struct number *lhs, struct number *rhs, struct state *s_lhs,
		struct state *s_rhs);

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
