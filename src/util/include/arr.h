#ifndef XR0_UTIL_ARR_H
#define XR0_UTIL_ARR_H

struct arr;

struct arr *
arr_create(void);

struct arr *
arr_copy(struct arr *, void *copy(void *));

struct arr *
arr_concat(struct arr *, struct arr *);

void
arr_destroy(struct arr *, void destroy(void *));

char *
arr_str(struct arr *, char *str(void *));

void
arr_append(struct arr *, void *);

void
arr_appendall(struct arr *, struct arr *);

int
arr_len(struct arr *);

void *
arr_get(struct arr *, int);

#endif
