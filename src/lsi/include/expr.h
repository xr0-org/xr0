#ifndef XR0_LSI_EXPR_H
#define XR0_LSI_EXPR_H

struct expr;

struct expr *
expr_copy(struct expr *);

void
expr_destroy(struct expr *);

char *
expr_str(struct expr *);

#endif
