#ifndef XR0_LSI_RANGE_H
#define XR0_LSI_RANGE_H

struct lsi_range;

struct lsi_range *
lsi_range_create(int, int);

void
lsi_range_destroy(struct lsi_range *);

char *
lsi_range_str(struct lsi_range *);

#endif
