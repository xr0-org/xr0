#ifndef XR0_LSI_LE_H
#define XR0_LSI_LE_H

struct lsi_le;

struct lsi_le *
lsi_le_copy(struct lsi_le *);

void
lsi_le_destroy(struct lsi_le *);

#endif
