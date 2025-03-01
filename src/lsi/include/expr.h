#ifndef XR0_LSI_EXPR_H
#define XR0_LSI_EXPR_H

struct lsi_expr;

struct lsi_expr *
lsi_expr_copy(struct lsi_expr *);

void
lsi_expr_destroy(struct lsi_expr *);

char *
lsi_expr_str(struct lsi_expr *);

#endif
