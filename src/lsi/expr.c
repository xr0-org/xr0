#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "ring_expr.h"

struct lsi_expr {
	enum type { CONST, VAR, RING } t;
	union {
		int c;
		char *s;
		struct ring_expr *r;
	};
};

static struct lsi_expr *
_expr_create(enum type t)
{
	struct lsi_expr *e = malloc(sizeof(struct lsi_expr));
	assert(e);
	e->t = t;
	return e;
}

struct lsi_expr *
lsi_expr_const_create(int c)
{
	struct lsi_expr *e = _expr_create(CONST);
	e->c = c;
	return e;
}

struct lsi_expr *
lsi_expr_var_create(char *s)
{
	struct lsi_expr *e = _expr_create(VAR);
	e->s = s;
	return e;
}

struct lsi_expr *
lsi_expr_sum_create(struct lsi_expr *e0, struct lsi_expr *e1)
{
	struct lsi_expr *e = _expr_create(RING);
	e->r = ring_expr_sum_create(e0, e1);
	return e;
}

struct lsi_expr *
lsi_expr_product_create(struct lsi_expr *e0, struct lsi_expr *e1)
{
	struct lsi_expr *e = _expr_create(RING);
	e->r = ring_expr_product_create(e0, e1);
	return e;
}

struct lsi_expr *
lsi_expr_copy(struct lsi_expr *old)
{
	struct lsi_expr *new = _expr_create(old->t);
	switch (old->t) {
	case CONST:
		new->c = old->c;
		break;
	case VAR:
		new->s = dynamic_str(old->s);
		break;
	case RING:
		new->r = ring_expr_copy(old->r);
		break;
	default:
		assert(0);
	}
	return new;
}

void
lsi_expr_destroy(struct lsi_expr *e)
{
	switch (e->t) {
	case CONST:
		break;
	case VAR:
		free(e->s);
		break;
	case RING:
		ring_expr_destroy(e->r);
		break;
	default:
		assert(0);
	}
	free(e);
}

static char *
_const_str(int);

char *
lsi_expr_str(struct lsi_expr *e)
{
	switch (e->t) {
	case CONST:
		return _const_str(e->c);
	case VAR:
		return dynamic_str(e->s);
	case RING:
		return ring_expr_str(e->r);
	default:
		assert(0);
	}
}

static char *
_const_str(int c)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%d", c);
	return strbuilder_build(b);
}
