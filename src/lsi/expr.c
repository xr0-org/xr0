#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "ring_expr.h"

struct expr {
	enum type { CONST, VAR, RING } t;
	union {
		int c;
		char *s;
		struct ring_expr *r;
	};
};

static struct expr *
_expr_create(enum type t)
{
	struct expr *e = malloc(sizeof(struct expr));
	assert(e);
	e->t = t;
	return e;
}

struct expr *
expr_const_create(int c)
{
	struct expr *e = _expr_create(CONST);
	e->c = c;
	return e;
}

struct expr *
expr_var_create(char *s)
{
	struct expr *e = _expr_create(VAR);
	e->s = s;
	return e;
}

struct expr *
expr_sum_create(struct expr *e0, struct expr *e1)
{
	struct expr *e = _expr_create(RING);
	e->r = ring_expr_sum_create(e0, e1);
	return e;
}

struct expr *
expr_product_create(struct expr *e0, struct expr *e1)
{
	struct expr *e = _expr_create(RING);
	e->r = ring_expr_product_create(e0, e1);
	return e;
}

struct expr *
expr_copy(struct expr *old)
{
	struct expr *new = _expr_create(old->t);
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
expr_destroy(struct expr *e)
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
expr_str(struct expr *e)
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
