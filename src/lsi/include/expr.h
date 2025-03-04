#ifndef XR0_LSI_EXPR_H
#define XR0_LSI_EXPR_H

struct lsi_expr;

struct lsi_expr *
lsi_expr_const_create(int);

struct lsi_expr *
lsi_expr_var_create(char *);

struct lsi_expr *
lsi_expr_sum_create(struct lsi_expr *, struct lsi_expr *);

struct lsi_expr *
lsi_expr_product_create(struct lsi_expr *, struct lsi_expr *);

struct lsi_expr *
_lsi_expr_copy(struct lsi_expr *);

void
_lsi_expr_destroy(struct lsi_expr *);

char *
lsi_expr_str(struct lsi_expr *);

struct lsi_expr *
_lsi_expr_varterms(struct lsi_expr *);

int
_lsi_expr_constterm(struct lsi_expr *);

struct string_arr;

struct string_arr *
_lsi_expr_getvars(struct lsi_expr *);

int
_lsi_expr_getcoef(struct lsi_expr *, char *var);

struct lsi_expr *
_lsi_expr_except(struct lsi_expr *, char *var);

#endif
