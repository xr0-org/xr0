#ifndef XR0_LSI_H
#define XR0_LSI_H

struct le_arr;

struct le_arr *
le_arr_create(void);

struct le_arr *
le_arr_copy(struct le_arr *);

void
le_arr_destroy(struct le_arr *);

struct le;

void
le_arr_append(struct le_arr *, struct le *);

int
le_arr_len(struct le_arr *);

struct le *
le_arr_get(struct le_arr *, int);

DECLARE_RESULT_TYPE(struct le_arr *, arr, le_arr_res)

struct le_arr_res *
le_arr_reduce(struct le_arr *);


/* le: a less-than-or-equal-to inequality */
struct le;

struct expr;

/* le_create: l <= r */
struct le *
le_create(struct expr *l, struct expr *r);

char *
le_str(struct le *);


struct expr;

struct expr *
expr_const_create(int);

struct expr *
expr_var_create(char *);

struct expr *
expr_sum_create(struct expr *, struct expr *);

struct expr *
expr_product_create(struct expr *, struct expr *);

#endif
