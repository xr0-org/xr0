#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "util.h"

#include "cconst.h"

struct cconst {
	enum type {
		CONSTANT,
		LIMIT,
	} t;
	union {
		int constant;
		int ismax;
	};
};

static struct cconst *
_cconst_create(enum type t)
{
	struct cconst *v = malloc(sizeof(struct cconst));
	assert(v);
	v->t = t;
	return v;
}

struct cconst *
cconst_constant_create(int constant)
{
	struct cconst *v = _cconst_create(CONSTANT);
	v->constant = constant;
	return v;
}

static struct cconst *
_cconst_limit_create(int ismax)
{
	struct cconst *v = _cconst_create(LIMIT);
	v->ismax = ismax;
	return v;
}

struct cconst *
cconst_min_create(void) { return _cconst_limit_create(0); }

struct cconst *
cconst_max_create(void) { return _cconst_limit_create(1); }

void
cconst_destroy(struct cconst *v)
{
	free(v);
}

char *
cconst_str(struct cconst *v)
{
	struct strbuilder *b = strbuilder_create();
	switch (v->t) {
	case CONSTANT:
		strbuilder_printf(b, "%d", v->constant);
		break;
	case LIMIT:
		strbuilder_printf(b, "%s", v->ismax ? "MAX" : "MIN");
		break;
	default:
		assert(0);
	}
	return strbuilder_build(b);
}

char *
cconst_str_inrange(struct cconst *v)
{
	switch (v->t) {
	case CONSTANT:
		return cconst_str(v);
	case LIMIT:
		return dynamic_str("");
	default:
		assert(0);
	}
}

struct cconst *
cconst_copy(struct cconst *v)
{
	switch (v->t) {
	case CONSTANT:
		return cconst_constant_create(v->constant);
	case LIMIT:
		return _cconst_limit_create(v->ismax);
	default:
		assert(0);
	}
}

int
cconsts_aresinglerange(struct cconst *lw, struct cconst *up)
{
	/* XXX: this omits the case where we have a constant value equal to one
	 * of the limits */
	if (lw->t != up->t) {
		return 0;
	}
	switch (lw->t) {
	case CONSTANT:
		return lw->constant == up->constant-1;
	case LIMIT:
		return lw->ismax == up->ismax;
	default:
		assert(0);
	}
}

int
cconst_difference(struct cconst *v1, struct cconst *v2)
{
	assert(v1->t == v2->t);

	switch (v1->t) {
	case CONSTANT:
		return v1->constant - v2->constant;
	default:
		assert(0);
	}
}

int
cconst_ismax(struct cconst *c) { return c->t == LIMIT && c->ismax; }

int
cconst_ismin(struct cconst *c) { return c->t == LIMIT && !c->ismax; }

int
cconst_isconstant(struct cconst *c) { return c->t == CONSTANT; }

int
cconst_as_constant(struct cconst *c)
{
	assert(cconst_isconstant(c));

	return c->constant;
}

int
cconst_le(struct cconst *v1, struct cconst *v2)
{
	if (v1->t != v2->t) {
		if (v1->t == LIMIT) {
			return !v1->ismax;
		}
		assert(v2->t == LIMIT);
		return v2->ismax;
	}
	assert(v1->t == v2->t);
	switch (v1->t) {
	case CONSTANT:
		return v1->constant <= v2->constant;
	case LIMIT:
		return !v1->ismax || v2->ismax;
	default:
		assert(0);
	}
}

int
cconst_ge(struct cconst *v1, struct cconst *v2)
{
	return cconst_le(v2, v1);
}

int
cconst_eq(struct cconst *v1, struct cconst *v2)
{
	return cconst_le(v1, v2) && cconst_le(v2, v1);
}

int
cconst_lt(struct cconst *v1, struct cconst *v2)
{
	return cconst_le(v1, v2) && !cconst_eq(v1, v2);
}
