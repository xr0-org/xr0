#ifndef XR0_LSI_LE_H
#define XR0_LSI_LE_H

struct le;

struct le *
le_copy(struct le *);

void
le_destroy(struct le *);

#endif
