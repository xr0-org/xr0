#ifndef XR0_LSI_LE_H
#define XR0_LSI_LE_H

struct lsi_le;

struct lsi_expr;

struct lsi_le *
lsi_le_create(struct lsi_expr *l, struct lsi_expr *r);

struct lsi_le *
_lsi_le_copy(struct lsi_le *);

void
_lsi_le_destroy(struct lsi_le *);

char *
lsi_le_str(struct lsi_le *);

struct string_arr;

struct string_arr *
_lsi_le_getvars(struct lsi_le *);

/* _lsi_le_getstdformcoef: standard-form coefficient. if the inequality is
 * written
 *
 * 	a_0 x_0 + a_1 x_1 + ··· + a_n b_n <= k,
 *
 * give the coefficient a_i of the variable x_i represented by var. this is
 * obviously zero is var doesn't appear. */
long
_lsi_le_getstdformcoef(struct lsi_le *, char *var);

/* _lsi_le_lowerbound: solve to place var alone on the rhs and return the lhs.
 * var must be in the inequality and have a positive sign on the rhs, or
 * assertion failure will result. */
struct lsi_expr *
_lsi_le_lowerbound(struct lsi_le *, char *var);

/* _lsi_le_upperbound: solve to place var alone on the rhs and return the lhs.
 * var must be in the inequality and have a positive sign on the lhs, or
 * assertion failure will result. */
struct lsi_expr *
_lsi_le_upperbound(struct lsi_le *, char *var);

/* _lsi_le_isfeasible: check feasibility, requiring all terms be constant. */
int
_lsi_le_isfeasible(struct lsi_le *);

#endif
