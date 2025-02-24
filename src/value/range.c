#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "util.h"
#include "value.h"

#include "cconst.h"
#include "number.h"
#include "range.h"

struct range {
	struct cconst *lower, *upper;
};

struct range *
range_create(struct cconst *lw, struct cconst *up)
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
		cconst_constant_create(cconst_as_constant(r->lower)+w),
		cconst_constant_create(cconst_as_constant(r->upper)+w)
	);
}


void
range_destroy(struct range *r)
{
	cconst_destroy(r->lower);
	cconst_destroy(r->upper);
	free(r);
}

struct cconst *
range_lower(struct range *r)
{
	return r->lower;
}

struct cconst *
range_upper(struct range *r)
{
	return r->upper;
}

char *
range_str(struct range *r)
{
	struct strbuilder *b = strbuilder_create();
	if (range_issingle(r)) {
		strbuilder_printf(b, "%s", cconst_str(r->lower));
	} else {
		strbuilder_printf(
			b, "%s?%s",
			cconst_str_inrange(r->lower),
			cconst_str_inrange(r->upper)
		);
	}
	return strbuilder_build(b);
}

struct range *
range_copy(struct range *r)
{
	return range_create(r->lower, r->upper);
}

int
range_contains_range(struct range *r, struct range *r2)
{
	if (cconst_le(r->lower, r2->lower)) {
		/* XXX: exclude partial inclusion cases */
		assert(r->upper);
		assert(r2->upper);
		assert(cconst_le(r2->upper, r->upper));
		/* ⊢ r->lower ≤ r2->lower && r2->upper ≤ r->upper */
		return 1;
	}
	/* r->lower > r2->lower so r2 cannot in be in r */
	return 0;
}

int
range_issingle(struct range *r)
{
	return cconsts_aresinglerange(r->lower, r->upper);
}

struct cconst *
range_as_cconst(struct range *r)
{
	assert(range_issingle(r));

	return r->lower;
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

static int
range_arr_containsrange(struct range_arr *, struct range *);

int
range_arr_containsrangearr(struct range_arr *arr, struct range_arr *range_arr)
{
	for (int i = 0; i < range_arr->n; i++) {
		if (!range_arr_containsrange(arr, range_arr->range[i])) {
			return 0;
		}
	}
	return 1;
}

static int
range_arr_containsrange(struct range_arr *arr, struct range *range)
{
	for (int i = 0; i < arr->n; i++) {
		/* XXX: currently will assert false if arr->range[i] contains
		 * the lower bound of range but not the entire range, so we're
		 * asserting out the possibility of partial inclusion */
		if (range_contains_range(arr->range[i], range)) {
			return 1;
		}
	}
	return 0;
}
