#ifndef REGISTERS_H
#define REGISTERS_H

#include <stdbool.h>

struct registers;

struct registers *
registers_create();

void
registers_destroy(struct registers *);

char *
registers_str(struct registers *);

struct registers *
registers_copy(struct registers *);

struct ast_expr *
registers_reserve(struct registers *);

void
registers_writeto(struct registers *, struct ast_expr *reg, struct value *);

struct value *
registers_readfrom(struct registers *, struct ast_expr *reg);

#endif
