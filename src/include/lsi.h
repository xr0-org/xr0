#ifndef XR0_LSI_H
#define XR0_LSI_H

/* lsi: a system of linear inequalities */
struct lsi;

struct lsi *
lsi_create(void);

struct lsi *
lsi_copy(struct lsi *);

void
lsi_destroy(struct lsi *);

char *
lsi_str(struct lsi *, char *prefix);

struct error;
struct lsi_le;

struct error *
lsi_add(struct lsi *, struct lsi_le *);


/* lsi_le: a less-than-or-equal-to inequality */
struct lsi_le;

struct lsi_expr;

/* le_create: l <= r */
struct lsi_le *
lsi_le_create(struct lsi_expr *l, struct lsi_expr *r);

char *
lsi_le_str(struct lsi_le *);


/* lsi_expr: an arithmetic term involving constants, variables, sums and
 * products */ 
struct lsi_expr;

struct lsi_expr *
lsi_expr_const_create(int);

struct lsi_expr *
lsi_expr_var_create(char *);

struct lsi_expr *
lsi_expr_sum_create(struct lsi_expr *, struct lsi_expr *);

struct lsi_expr *
lsi_expr_product_create(struct lsi_expr *, struct lsi_expr *);

char *
lsi_expr_str(struct lsi_expr *);

#endif
