#ifndef XR0_LSI_RING_EXPR_H
#define XR0_LSI_RING_EXPR_H

struct ring_expr;

struct expr;

struct ring_expr *
ring_expr_sum_create(struct expr *, struct expr *);

struct ring_expr *
ring_expr_product_create(struct expr *, struct expr *);

struct ring_expr *
ring_expr_copy(struct ring_expr *);

void
ring_expr_destroy(struct ring_expr *);

char *
ring_expr_str(struct ring_expr *);

#endif
