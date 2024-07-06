#ifndef XR0_PROPS_H
#define XR0_PROPS_H

#include <stdbool.h>

struct props;

struct props *
props_create();

struct props *
props_copy(struct props *);

void
props_destroy(struct props *);

char *
props_str(struct props *, char *indent);

struct ast_expr;

int
props_n(struct props *);

struct ast_expr **
props_props(struct props *);

void
props_install(struct props *, struct ast_expr *);

bool
props_get(struct props *, struct ast_expr *);

bool
props_contradicts(struct props *k, struct ast_expr *);

#endif
