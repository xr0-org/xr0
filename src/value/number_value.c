#include <stdlib.h>
#include <assert.h>

#include "util.h"

#include "number_value.h"

struct number_value {
	enum type {
		CONSTANT,
		LIMIT,
	} t;
	union {
		int constant;
		int ismax;
	};
};

static struct number_value *
_number_value_create(enum type t)
{
	struct number_value *v = malloc(sizeof(struct number_value));
	assert(v);
	v->t = t;
	return v;
}

struct number_value *
number_value_constant_create(int constant)
{
	struct number_value *v = _number_value_create(CONSTANT);
	v->constant = constant;
	return v;
}

static struct number_value *
_number_value_limit_create(int ismax)
{
	struct number_value *v = _number_value_create(LIMIT);
	v->ismax = ismax;
	return v;
}

struct number_value *
number_value_min_create(void) { return _number_value_limit_create(0); }

struct number_value *
number_value_max_create(void) { return _number_value_limit_create(1); }

void
number_value_destroy(struct number_value *v)
{
	free(v);
}

char *
number_value_str(struct number_value *v)
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
number_value_str_inrange(struct number_value *v)
{
	switch (v->t) {
	case CONSTANT:
		return number_value_str(v);
	case LIMIT:
		return dynamic_str("");
	default:
		assert(0);
	}
}

struct number_value *
number_value_copy(struct number_value *v)
{
	switch (v->t) {
	case CONSTANT:
		return number_value_constant_create(v->constant);
	case LIMIT:
		return _number_value_limit_create(v->ismax);
	default:
		assert(0);
	}
}

int
number_values_aresingle(struct number_value *v1, struct number_value *v2)
{
	/* XXX: this omits the case where we have a constant value equal to one
	 * of the limits */
	if (v1->t != v2->t) {
		return 0;
	}
	switch (v1->t) {
	case CONSTANT:
		return v1->constant == v2->constant-1;
	case LIMIT:
		return v1->ismax == v2->ismax;
	default:
		assert(0);
	}
}

int
number_value_difference(struct number_value *v1, struct number_value *v2)
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
number_value_as_constant(struct number_value *v)
{
	assert(v->t == CONSTANT);

	return v->constant;
}

int
number_value_le(struct number_value *v1, struct number_value *v2)
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
number_value_ge(struct number_value *v1, struct number_value *v2)
{
	return number_value_le(v2, v1);
}

int
number_value_eq(struct number_value *v1, struct number_value *v2)
{
	return number_value_le(v1, v2) && number_value_le(v2, v1);
}

int
number_value_lt(struct number_value *v1, struct number_value *v2)
{
	return number_value_le(v1, v2) && !number_value_eq(v1, v2);
}
