#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "expr.h"

struct ring_expr {
	enum op { PLUS, TIMES } op;
	struct lsi_expr *e0, *e1;
};

static struct ring_expr *
_ring_expr_create(struct lsi_expr *e0, enum op op, struct lsi_expr *e1)
{
	struct ring_expr *e = malloc(sizeof(struct ring_expr));
	assert(e);
	e->e0 = e0;
	e->op = op;
	e->e1 = e1;
	return e;
}

struct ring_expr *
ring_expr_sum_create(struct lsi_expr *e0, struct lsi_expr *e1)
{
	return _ring_expr_create(e0, PLUS, e1);
}

struct ring_expr *
ring_expr_product_create(struct lsi_expr *e0, struct lsi_expr *e1)
{
	return _ring_expr_create(e0, TIMES, e1);
}

struct ring_expr *
ring_expr_copy(struct ring_expr *e)
{
	return _ring_expr_create(
		_lsi_expr_copy(e->e0), e->op, _lsi_expr_copy(e->e1)
	);
}

void
ring_expr_destroy(struct ring_expr *e)
{
	_lsi_expr_destroy(e->e0);
	_lsi_expr_destroy(e->e1);
	free(e);
}

static char
_op_char(enum op); 

char *
ring_expr_str(struct ring_expr *e)
{
	struct strbuilder *b = strbuilder_create();
	char *e0 = lsi_expr_str(e->e0),
	     *e1 = lsi_expr_str(e->e1);
	strbuilder_printf(b, "(%s%c%s)", e0, _op_char(e->op), e1);
	free(e1);
	free(e0);
	return strbuilder_build(b);
}

static char
_op_char(enum op op)
{
	switch (op) {
	case PLUS:
		return '+';
	case TIMES:
		return '*';
	default:
		assert(0);
	}
}
