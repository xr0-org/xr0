#ifndef XR0_LSI_EXPR_H
#define XR0_LSI_EXPR_H

struct lsi_expr;

struct tally;

struct lsi_expr *
_lsi_expr_tally_create(struct tally *);

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

struct tally *
_lsi_expr_tally(struct lsi_expr *);

#endif
