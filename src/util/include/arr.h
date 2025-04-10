#ifndef XR0_UTIL_ARR_H
#define XR0_UTIL_ARR_H

struct arr;

struct arr *
arr_create(void);

struct any;

struct arr *
arr_copy(struct arr *, struct any *copy(struct any *));

struct arr *
arr_map(struct arr *, struct any *map(struct any *));

/* arr_do: apply f to the elements of the array, in sequence. */
void
arr_do(struct arr *, void f(struct any *));

struct arr *
arr_concat(struct arr *, struct arr *);

void
arr_destroy(struct arr *, void destroy(struct any *));

void
arr_append(struct arr *, struct any *);

void
arr_appendall(struct arr *, struct arr *);

int
arr_len(struct arr *);

struct any *
arr_get(struct arr *, int);

#endif
