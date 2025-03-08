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

struct error *
lsi_addrange(struct lsi *, struct lsi *);

struct lsi_varmap;

struct lsi *
lsi_renamevars(struct lsi *, struct lsi_varmap *);

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


/* lsi_varmap: a name->name mapping of variables */
struct lsi_varmap;

struct lsi_varmap *
lsi_varmap_create(void);

struct lsi_varmap *
lsi_varmap_copy(struct lsi_varmap *);

void
lsi_varmap_destroy(struct lsi_varmap *);

char *
lsi_varmap_str(struct lsi_varmap *);

void
lsi_varmap_set(struct lsi_varmap *, char *k, char *v);

/* lsi_varmap_addrange: copy m0's key-value pairs to m, asserting that any common
 * keys must have the same value in both maps. */
void
lsi_varmap_addrange(struct lsi_varmap *m, struct lsi_varmap *m0);

#endif
