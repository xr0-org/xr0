#ifndef XR0_LSI_EXPR_ARR_H
#define XR0_LSI_EXPR_ARR_H

struct expr_arr;

struct expr_arr *
expr_arr_create(void);

struct expr_arr *
expr_arr_copy(struct expr_arr *);

void
expr_arr_destroy(struct expr_arr *);

struct lsi_expr;

void
expr_arr_append(struct expr_arr *, struct lsi_expr *);

int
expr_arr_len(struct expr_arr *);

struct lsi_expr *
expr_arr_get(struct expr_arr *, int);

#endif
