#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "math.h"
#include "util.h"
#include "long-map.h"

bool
math_eq(struct math_expr *e1, struct math_expr *e2)
{
	return math_le(e1, e2) && math_le(e2, e1);
}

bool
math_lt(struct math_expr *e1, struct math_expr *e2)

{
	return math_le(e1, e2) && !math_eq(e1, e2);
}

bool
math_gt(struct math_expr *e1, struct math_expr *e2)
{
	return math_lt(e2, e1);
}

bool
math_ge(struct math_expr *e1, struct math_expr *e2)
{
	return math_le(e2, e1);
}

struct tally {
	struct long_map *map;
	int num;
};

static struct tally
tally(struct math_expr *);

static struct math_expr *
math_expr_fromint(int i)
{
	if (i < 0) {
		return math_expr_neg_create(math_expr_fromint(-i));
	}
	return math_expr_atom_create(math_atom_nat_create(i));
}

static struct math_expr *
math_expr_fromvartally(char *id, int num)
{
	assert(num != 0);

	if (num < 0) {
		return math_expr_neg_create(math_expr_fromvartally(id, -num));
	}

	struct math_expr *e = math_expr_atom_create(math_atom_variable_create(id));

	for (int i = 0; i < num-1; i++) {
		e = math_expr_sum_create(
			e, math_expr_atom_create(math_atom_variable_create(id))
		);
	}

	return e;
}

static bool
variable_tally_eq(struct long_map *, struct long_map *);

bool
math_le(struct math_expr *e1, struct math_expr *e2)
{
	struct tally d1 = tally(e1),
		     d2 = tally(e2);

	return variable_tally_eq(d1.map, d2.map) && d1.num <= d2.num;
}

struct math_expr {
	enum expr_type {
		EXPR_ATOM, EXPR_SUM, EXPR_NEG,
	} type;
	union {
		struct math_atom *a;
		struct {
			struct math_expr *e1, *e2;
		} sum;
		struct math_expr *negated;
	};
};

struct math_expr *
math_expr_atom_create(struct math_atom *a)
{
	struct math_expr *e = malloc(sizeof(struct math_expr));
	e->type = EXPR_ATOM;
	e->a = a;
	return e;
}

struct math_expr *
math_expr_sum_create(struct math_expr *e1, struct math_expr *e2)
{
	struct math_expr *e = malloc(sizeof(struct math_expr));
	e->type = EXPR_SUM;
	e->sum.e1 = e1;
	e->sum.e2 = e2;
	return e;
}


struct math_expr *
math_expr_neg_create(struct math_expr *orig)
{
	struct math_expr *e = malloc(sizeof(struct math_expr));
	e->type = EXPR_NEG;
	e->negated = orig;
	return e;
}


struct math_expr *
math_expr_copy(struct math_expr *e)
{
	switch (e->type) {
	case EXPR_ATOM:
		return math_expr_atom_create(math_atom_copy(e->a));
	case EXPR_SUM:
		return math_expr_sum_create(
			math_expr_copy(e->sum.e1),
			math_expr_copy(e->sum.e2)
		);
	default:
		assert(false);
	}
}

void
math_expr_destroy(struct math_expr *e)
{
	switch (e->type) {
	case EXPR_ATOM:
		math_atom_destroy(e->a);
		break;
	case EXPR_SUM:
		math_expr_destroy(e->sum.e1);
		math_expr_destroy(e->sum.e2);
		break;
	case EXPR_NEG:
		math_expr_destroy(e->negated);
		break;
	default:
		assert(false);
	}
	free(e);
}

static char * 
math_expr_sum_str(struct math_expr *);

static char * 
math_expr_neg_str(struct math_expr *);

char *
math_expr_str(struct math_expr *e)
{
	switch (e->type) {
	case EXPR_ATOM:
		return math_atom_str(e->a);
	case EXPR_SUM:
		return math_expr_sum_str(e);
	case EXPR_NEG:
		return math_expr_neg_str(e);
	default:
		assert(false);
	}

}

static char * 
math_expr_sum_str(struct math_expr *e)
{
	struct strbuilder *b = strbuilder_create();

	char *e1 = math_expr_str(e->sum.e1),
	     *e2 = math_expr_str(e->sum.e2);

	char *sign = (*e2 == '-') ? "" : "+";
	strbuilder_printf(b, "(%s%s%s)", e1, sign, e2);

	free(e2); free(e1);

	return strbuilder_build(b);
}

static char * 
math_expr_neg_str(struct math_expr *e)
{
	struct strbuilder *b = strbuilder_create();

	char *orig = math_expr_str(e->negated);

	strbuilder_printf(b, "-%s", orig);

	free(orig);

	return strbuilder_build(b);
}

static struct math_expr *
math_expr_nullablesum(struct math_expr *e1, struct math_expr *e2)
{
	if (!e1) {
		return e2 ? e2 : math_expr_fromint(0);
	}
	return e2 ? math_expr_sum_create(e1, e2) : e1;
}

struct math_expr *
math_expr_simplify(struct math_expr *raw)
{
	struct tally t = tally(raw);

	struct long_map *m = t.map;

	struct math_expr *expr = NULL;

	for (int i = 0; i < m->n; i++) {
		struct long_map_entry e = m->entry[i];
		if (!e.value) {
			continue;
		}
		expr = math_expr_nullablesum(
			expr, math_expr_fromvartally(e.key, (long) e.value)
		);
	}

	struct math_expr *num = t.num ? math_expr_fromint(t.num) : NULL;

	expr = math_expr_nullablesum(expr, num);

	assert(expr);

	return expr;
}

static struct tally
atom_tally(struct math_atom *);

static struct tally
sum_tally(struct math_expr *e);

static struct tally
neg_tally(struct math_expr *e);

static struct tally
tally(struct math_expr *e)
{
	switch (e->type) {
	case EXPR_ATOM:
		return atom_tally(e->a);
	case EXPR_SUM:
		return sum_tally(e);
	case EXPR_NEG:
		return neg_tally(e);
	default:
		assert(false);
	}
}

static struct long_map *
map_sum(struct long_map *, struct long_map *);

static struct tally
sum_tally(struct math_expr *e)
{
	assert(e->type == EXPR_SUM);

	struct tally r1 = tally(e->sum.e1),
		     r2 = tally(e->sum.e2);

	return (struct tally) {
		map_sum(r1.map, r2.map), r1.num + r2.num,
	};
}

static struct long_map *
map_sum(struct long_map *m1, struct long_map *m2)
{
	struct long_map *m = long_map_create();

	for (int i = 0; i < m1->n; i++) {
		struct long_map_entry e = m1->entry[i];
		long_map_set(m, dynamic_str(e.key), e.value);
	}
	for (int i = 0; i < m2->n; i++) {
		struct long_map_entry e = m2->entry[i];
		long_map_set(m, dynamic_str(e.key), long_map_get(m, e.key) + e.value);
	}

	long_map_destroy(m2);
	long_map_destroy(m1);

	return m;
}


static struct tally
neg_tally(struct math_expr *e)
{
	struct tally r = tally(e->negated);

	struct long_map *m = r.map;
	for (int i = 0; i < m->n; i++) {
		struct long_map_entry e = m->entry[i];
		long val = long_map_get(m, e.key);
		long_map_set(m, e.key, val);
	}
	r.num = -r.num;

	return r;
}

static bool
variable_tally_eq(struct long_map *m1, struct long_map *m2)
{
	bool res = false;
	for (int i = 0; i < m1->n; i++) {
		char *key = m1->entry[i].key;
		if (long_map_get(m1, key) != long_map_get(m2, key)) {
			res = false;
		}
	}
	res = m1->n == m2->n;

	long_map_destroy(m2);
	long_map_destroy(m1);

	return res;
}

struct math_atom {
	enum atom_type {
		ATOM_NAT, ATOM_VARIABLE,
	} type;
	union {
		unsigned int i;
		char *v;
	};
};

struct math_atom *
math_atom_nat_create(unsigned int i)
{
	struct math_atom *a = malloc(sizeof(struct math_atom));
	a->type = ATOM_NAT;
	a->i = i;
	return a;
}

struct math_atom *
math_atom_variable_create(char *s)
{
	struct math_atom *a = malloc(sizeof(struct math_atom));
	a->type = ATOM_VARIABLE;
	a->v = s;
	return a;
}

struct math_atom *
math_atom_copy(struct math_atom *a)
{
	switch (a->type) {
	case ATOM_NAT:
		return math_atom_nat_create(a->i);
	case ATOM_VARIABLE:
		return math_atom_variable_create(dynamic_str(a->v));
	default:
		assert(false);
	}
}

void
math_atom_destroy(struct math_atom *a)
{
	switch (a->type) {
	case ATOM_NAT:
		break;
	case ATOM_VARIABLE:
		free(a->v);
		break;
	default:
		assert(false);
	}
	free(a);
}

char *
math_atom_str(struct math_atom *a)
{
	if (a->type == ATOM_VARIABLE) {
		return dynamic_str(a->v);
	}

	assert(a->type == ATOM_NAT);

	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%d", a->i);
	return strbuilder_build(b);
}

static struct long_map *
map_fromvar(char *id);

static struct tally
atom_tally(struct math_atom *a)
{
	switch (a->type) {
	case ATOM_NAT:
		return (struct tally) { long_map_create(), a->i };
	case ATOM_VARIABLE:
		return (struct tally) {
			map_fromvar(dynamic_str(a->v)), 0
		};
	default:
		assert(false);
	}
}

static struct long_map *
map_fromvar(char *id)
{
	struct long_map *m = long_map_create();
	long_map_set(m, id, 1);
	return m;
}
