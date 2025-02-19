#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "util.h"
#include "value.h"

#include "cconst.h"
#include "number.h"
#include "range.h"

struct range {
	struct number *lower, *upper;
};

struct range *
range_create(struct number *lw, struct number *up)
{
	struct range *r = malloc(sizeof(struct range));
	r->lower = lw;
	r->upper = up;
	return r;
}

struct range *
range_shift(struct range *r, int w)
{
	return range_create(
		number_cconst_create(
			cconst_constant_create(
				cconst_as_constant(number_as_cconst(r->lower))+w
			)
		),
		number_cconst_create(
			cconst_constant_create(
				cconst_as_constant(number_as_cconst(r->upper))+w
			)
		)
	);
}


void
range_destroy(struct range *r)
{
	number_destroy(r->lower);
	number_destroy(r->upper);
	free(r);
}

struct number *
range_lower(struct range *r)
{
	return r->lower;
}

struct number *
range_upper(struct range *r)
{
	return r->upper;
}

char *
range_str(struct range *r)
{
	struct strbuilder *b = strbuilder_create();
	if (range_issingle(r)) {
		strbuilder_printf(b, "%s", number_str(r->lower));
	} else {
		strbuilder_printf(
			b, "%s?%s",
			number_str_inrange(r->lower),
			number_str_inrange(r->upper)
		);
	}
	return strbuilder_build(b);
}

struct range *
range_copy(struct range *r)
{
	return range_create(
		number_copy(r->lower), number_copy(r->upper)
	);
}

int
range_contains_range(struct range *r, struct range *r2, struct state *s)
{
	if (number_le(r->lower, r2->lower, s)) {
		/* XXX: exclude partial inclusion cases */
		assert(r->upper);
		assert(r2->upper);
		assert(number_le(r2->upper, r->upper, s));
		/* ⊢ r->lower ≤ r2->lower && r2->upper ≤ r->upper */
		return true;
	}
	/* r->lower > r2->lower so r2 cannot in be in r */
	return false;
}

int
range_issingle(struct range *r)
{
	return numbers_aresinglerange(r->lower, r->upper);
}

struct cconst *
range_as_cconst(struct range *r)
{
	assert(range_issingle(r));

	return number_as_cconst(r->lower);
}

struct range_arr {
	int n;
	struct range **range;
};

struct range_arr *
range_arr_create(void)
{
	struct range_arr *arr = calloc(1, sizeof(struct range_arr));
	assert(arr);
	return arr;
}

struct range_arr *
range_arr_copy(struct range_arr *old)
{
	struct range_arr *new = range_arr_create();
	for (int i = 0; i < old->n; i++) {
		range_arr_append(new, range_copy(old->range[i]));
	}
	return new;
}

void
range_arr_destroy(struct range_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		range_destroy(arr->range[i]);	
	}
	free(arr->range);
	free(arr);
}

int
range_arr_n(struct range_arr *arr)
{
	return arr->n;
}

struct range **
range_arr_range(struct range_arr *arr)
{
	return arr->range;
}

int
range_arr_append(struct range_arr *arr, struct range *r)
{
	arr->range = realloc(arr->range, sizeof(struct range *) * ++arr->n);
	assert(arr->range);
	int loc = arr->n-1;
	arr->range[loc] = r;
	return loc;
}

static bool
range_arr_containsrange(struct range_arr *, struct range *, struct state *);

int
range_arr_containsrangearr(struct range_arr *arr, struct range_arr *range_arr,
		struct state *s)
{
	for (int i = 0; i < range_arr->n; i++) {
		if (!range_arr_containsrange(arr, range_arr->range[i], s)) {
			return 0;
		}
	}
	return 1;
}

static bool
range_arr_containsrange(struct range_arr *arr, struct range *range,
		struct state *s)
{
	for (int i = 0; i < arr->n; i++) {
		/* XXX: currently will assert false if arr->range[i] contains
		 * the lower bound of range but not the entire range, so we're
		 * asserting out the possibility of partial inclusion */
		if (range_contains_range(arr->range[i], range, s)) {
			return true;
		}
	}
	return false;
}
