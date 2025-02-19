#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "util.h"
#include "value.h"

#include "cconst.h"
#include "range.h"

struct number_range {
	struct number *lower, *upper;
};

struct number_range *
number_range_create(struct number *lw, struct number *up)
{
	struct number_range *r = malloc(sizeof(struct number_range));
	r->lower = lw;
	r->upper = up;
	return r;
}

void
number_range_destroy(struct number_range *r)
{
	number_destroy(r->lower);
	number_destroy(r->upper);
	free(r);
}

struct number *
number_range_lower(struct number_range *r)
{
	return r->lower;
}

struct number *
number_range_upper(struct number_range *r)
{
	return r->upper;
}

static int
number_range_issingle(struct number_range *r);

char *
number_range_str(struct number_range *r)
{
	struct strbuilder *b = strbuilder_create();
	if (number_range_issingle(r)) {
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

struct number_range *
number_range_copy(struct number_range *r)
{
	return number_range_create(
		number_copy(r->lower), number_copy(r->upper)
	);
}

int
number_range_contains_range(struct number_range *r, struct number_range *r2)
{
	if (number_le(r->lower, r2->lower)) {
		/* XXX: exclude partial inclusion cases */
		assert(r->upper);
		assert(r2->upper);
		assert(number_le(r2->upper, r->upper));
		/* ⊢ r->lower ≤ r2->lower && r2->upper ≤ r->upper */
		return true;
	}
	/* r->lower > r2->lower so r2 cannot in be in r */
	return false;
}

static int
number_range_issingle(struct number_range *r)
{
	return numbers_aresinglerange(r->lower, r->upper);
}

int
number_range_equal(struct number_range *r1, struct number_range *r2)
{
	return number_eq(r1->lower, r2->lower)
		&& number_eq(r1->upper, r2->upper);
}

struct cconst *
number_range_as_cconst(struct number_range *r)
{
	assert(number_range_issingle(r));

	return number_as_cconst(r->lower);
}

struct number_range_arr {
	int n;
	struct number_range **range;
};

struct number_range_arr *
number_range_arr_create(void)
{
	struct number_range_arr *arr = calloc(1, sizeof(struct number_range_arr));
	assert(arr);
	return arr;
}

struct number_range_arr *
number_range_arr_copy(struct number_range_arr *old)
{
	struct number_range_arr *new = number_range_arr_create();
	for (int i = 0; i < old->n; i++) {
		number_range_arr_append(new, number_range_copy(old->range[i]));
	}
	return new;
}

void
number_range_arr_destroy(struct number_range_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		number_range_destroy(arr->range[i]);	
	}
	free(arr->range);
	free(arr);
}

int
number_range_arr_n(struct number_range_arr *arr)
{
	return arr->n;
}

struct number_range **
number_range_arr_range(struct number_range_arr *arr)
{
	return arr->range;
}

int
number_range_arr_append(struct number_range_arr *arr, struct number_range *r)
{
	arr->range = realloc(arr->range, sizeof(struct number_range *) * ++arr->n);
	assert(arr->range);
	int loc = arr->n-1;
	arr->range[loc] = r;
	return loc;
}

struct ast_expr *
number_ranges_to_expr(struct number_range_arr *arr)
{
	assert(number_range_arr_n(arr) == 1);
	struct number_range *r = arr->range[0];
	assert(number_range_issingle(r));

	return ast_expr_constant_create(
		cconst_as_constant(number_as_cconst(number_range_lower(r)))
	);
}

static bool
number_range_arr_containsrange(struct number_range_arr *, struct number_range *);

int
number_range_arr_containsrangearr(struct number_range_arr *arr,
		struct number_range_arr *range_arr)
{
	for (int i = 0; i < range_arr->n; i++) {
		if (!number_range_arr_containsrange(arr, range_arr->range[i])) {
			return 0;
		}
	}
	return 1;
}

static bool
number_range_arr_containsrange(struct number_range_arr *arr,
		struct number_range *range)
{
	for (int i = 0; i < arr->n; i++) {
		/* XXX: currently will assert false if arr->range[i] contains
		 * the lower bound of range but not the entire range, so we're
		 * asserting out the possibility of partial inclusion */
		if (number_range_contains_range(arr->range[i], range)) {
			return true;
		}
	}
	return false;
}
