#ifndef XR0_VALUE_RANGE_H
#define XR0_VALUE_RANGE_H

struct number_range;

struct number;

struct number_range *
number_range_create(struct number *lw, struct number *up);

struct number_range *
number_range_copy(struct number_range *);

void
number_range_destroy(struct number_range *);

char *
number_range_str(struct number_range *r);

struct number *
number_range_lower(struct number_range *);

struct number *
number_range_upper(struct number_range *);

int
number_range_contains_range(struct number_range *r, struct number_range *r2);


struct number_range_arr;

struct number_range_arr *
number_range_arr_create(void);

struct number_range_arr *
number_range_arr_copy(struct number_range_arr *);

void
number_range_arr_destroy(struct number_range_arr *arr);

int
number_range_arr_n(struct number_range_arr *);

struct number_range **
number_range_arr_range(struct number_range_arr *);

int
number_range_arr_append(struct number_range_arr *, struct number_range *);

int
number_range_arr_containsrangearr(struct number_range_arr *arr,
		struct number_range_arr *range);

#endif
