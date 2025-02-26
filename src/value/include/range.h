#ifndef XR0_VALUE_RANGE_H
#define XR0_VALUE_RANGE_H

struct range;

struct range *
range_create(long lw, long up);

struct range *
range_entire_create(void);

struct range *
range_copy(struct range *);

void
range_destroy(struct range *);

char *
range_str(struct range *r);

char *
range_short_str(struct range *r);

long
range_lower(struct range *);

long
range_upper(struct range *);

long
range_as_const(struct range *);

struct state;

int
range_contains_range(struct range *r, struct range *r2);

int
range_issingle(struct range *r);

struct range_arr;

struct range_arr *
range_arr_create(void);

struct range_arr *
range_arr_copy(struct range_arr *);

void
range_arr_destroy(struct range_arr *arr);

int
range_arr_n(struct range_arr *);

struct range **
range_arr_range(struct range_arr *);

int
range_arr_append(struct range_arr *, struct range *);

int
range_arr_containsrangearr(struct range_arr *arr, struct range_arr *range);

#endif
