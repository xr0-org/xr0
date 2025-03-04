#ifndef XR0_LSI_LE_ARR_H
#define XR0_LSI_LE_ARR_H

struct le_arr;

struct le_arr *
le_arr_create(void);

struct le_arr *
le_arr_copy(struct le_arr *);

void
le_arr_destroy(struct le_arr *);

struct lsi_le;

void
le_arr_append(struct le_arr *, struct lsi_le *);

int
le_arr_len(struct le_arr *);

struct lsi_le *
le_arr_get(struct le_arr *, int);

#endif
