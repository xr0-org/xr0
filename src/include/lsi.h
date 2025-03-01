#ifndef XR0_LSI_H
#define XR0_LSI_H

struct lsi_le_arr;

struct lsi_le_arr *
lsi_le_arr_create(void);

struct lsi_le_arr *
lsi_le_arr_copy(struct lsi_le_arr *);

void
lsi_le_arr_destroy(struct lsi_le_arr *);

struct lsi_le;

void
lsi_le_arr_append(struct lsi_le_arr *, struct lsi_le *);

int
lsi_le_arr_len(struct lsi_le_arr *);

struct lsi_le *
lsi_le_arr_get(struct lsi_le_arr *, int);

DECLARE_RESULT_TYPE(struct lsi_le_arr *, arr, lsi_le_arr_res)

struct lsi_le_arr_res *
lsi_le_arr_reduce(struct lsi_le_arr *);


/* lsi_le: a less-than-or-equal-to inequality */
struct lsi_le;

struct lsi_expr;

/* le_create: l <= r */
struct lsi_le *
lsi_le_create(struct lsi_expr *l, struct lsi_expr *r);

char *
lsi_le_str(struct lsi_le *);


struct lsi_expr;

struct lsi_expr *
lsi_expr_const_create(int);

struct lsi_expr *
lsi_expr_var_create(char *);

struct lsi_expr *
lsi_expr_sum_create(struct lsi_expr *, struct lsi_expr *);

struct lsi_expr *
lsi_expr_product_create(struct lsi_expr *, struct lsi_expr *);

#endif
