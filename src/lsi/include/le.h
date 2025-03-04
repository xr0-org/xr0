#ifndef XR0_LSI_LE_H
#define XR0_LSI_LE_H

struct lsi_le;

struct lsi_expr;

struct lsi_le *
lsi_le_create(struct lsi_expr *l, struct lsi_expr *r);

struct lsi_le *
_lsi_le_copy(struct lsi_le *);

void
_lsi_le_destroy(struct lsi_le *);

char *
lsi_le_str(struct lsi_le *);

struct string_arr;

struct string_arr *
_lsi_le_getvars(struct lsi_le *);

#endif
