#ifndef XR0_PROPS_H
#define XR0_PROPS_H

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

void
props_install(struct props *, struct ast_expr *);

#endif
