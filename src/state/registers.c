#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "value.h"
#include "registers.h"

struct registers {
	int n;
	struct value **reg;
};

struct registers *
registers_create()
{
	struct registers *r = malloc(sizeof(struct registers));
	r->n = 0;
	r->reg = malloc(sizeof(struct value *));
	return r;
}

void
registers_destroy(struct registers *r)
{
	assert(false);
}

char *
registers_str(struct registers *r)
{
	struct strbuilder *b = strbuilder_create();
	for (int i = 0; i < r->n; i++) {
		strbuilder_printf(b, "\t%d: %s\n", value_str(r->reg[i]));
	}
	return strbuilder_build(b);
}

struct registers *
registers_copy(struct registers *old)
{
	struct registers *new = registers_create();
	if (!old->reg) {
		return new;
	}
	for (int i = 0; i < old->n; i++) {
		struct ast_expr *reg = registers_reserve(new);
		registers_writeto(new, reg, value_copy(old->reg[i]));
	}
	return new;
}

struct ast_expr *
registers_reserve(struct registers *r)
{	
	r->reg = realloc(r->reg, sizeof(struct value *) * r->n++);
	return ast_expr_register_create(r->n-1);
}

void
registers_writeto(struct registers *r, struct ast_expr *reg, struct value *val)
{
	assert(r->n > ast_expr_register_slot(reg));
	r->reg[ast_expr_register_slot(reg)] = val;
}

struct value *
registers_readfrom(struct registers *r, struct ast_expr *reg)
{
	assert(r->n > ast_expr_register_slot(reg));
	struct value *v = r->reg[ast_expr_register_slot(reg)];
	assert(v);
	return v;
}
