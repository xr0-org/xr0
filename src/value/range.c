#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "util.h"
#include "value.h"

#include "number.h"
#include "range.h"
#include "_limits.h"

struct range {
	long lower, upper;
};

static int
_isint(long);

struct range *
range_create(long lw, long up)
{
	a_printf(
		_isint(lw) && _isint(up), 
		"only values between INT_MIN and INT_MAX are supported\n"
	);
	assert(lw < up);

	struct range *r = malloc(sizeof(struct range));
	assert(r);
	r->lower = lw;
	r->upper = up;
	return r;
}

static int
_isint(long l) { return C89_INT_MIN <= l && l <= C89_INT_MAX+2; }

struct range *
range_entire_create(void) { return range_create(C89_INT_MIN, C89_INT_MAX+1); }

static long
_lw_bound_value(struct ast_expr *, struct state *);

static long
_up_bound_value(struct ast_expr *, struct state *);

struct range *
range_fromexpr(struct ast_expr *e, struct state *s)
{
	return range_create(
		_lw_bound_value(ast_expr_range_lw(e), s),
		_up_bound_value(ast_expr_range_up(e), s)
	);
}

static int
_eval(struct ast_expr *, struct state *);

static long
_lw_bound_value(struct ast_expr *e, struct state *s)
{
	return ast_expr_israngemin(e) ? C89_INT_MIN : _eval(e, s);
}

static long
_up_bound_value(struct ast_expr *e, struct state *s)
{
	return ast_expr_israngemax(e) ? C89_INT_MAX+1 : _eval(e, s);
}

static int
_eval(struct ast_expr *e, struct state *s)
{
	return value_as_int(
		value_res_as_value(
			eval_to_value(e_res_as_eval(ast_expr_eval(e, s)), s)
		), s
	);
}

struct range *
range_copy(struct range *r) { return range_create(r->lower, r->upper); }

void
range_destroy(struct range *r) { free(r); }

static char *
_limit_str(long);

char *
range_str(struct range *r)
{
	struct strbuilder *b = strbuilder_create();
	char *lw = _limit_str(r->lower),
	     *up = _limit_str(r->upper);
	strbuilder_printf(b, "%s?%s", lw, up);
	free(up);
	free(lw);
	return strbuilder_build(b);
}

static char *
_limit_str(long l)
{
	struct strbuilder *b = strbuilder_create();
	switch (l) {
	case C89_INT_MIN:
	case C89_INT_MAX+1:
		break;
	default:
		strbuilder_printf(b, "%d", l);
	}
	return strbuilder_build(b);
}

static char *
_single_str(long);

char *
range_short_str(struct range *r)
{
	return range_issingle(r) ? _single_str(r->lower) : range_str(r);
}

static char *
_single_str(long l)
{
	struct strbuilder *b = strbuilder_create();
	switch (l) {
	case C89_INT_MIN:
		strbuilder_printf(b, "INT_MIN");
		break;
	case C89_INT_MAX:
		strbuilder_printf(b, "INT_MAX");
		break;
	default:
		strbuilder_printf(b, "%d", l);
	}
	return strbuilder_build(b);
}

long
range_lower(struct range *r) { return r->lower; }

long
range_upper(struct range *r) { return r->upper; }

long
range_as_const(struct range *r)
{
	assert(range_issingle(r));
	return r->lower;
}

int
range_contains_range(struct range *r, struct range *r2)
{
	if (r->lower <= r2->lower) {
		assert(r2->upper <= r->upper);
		/* ⊢ r->lower ≤ r2->lower && r2->upper ≤ r->upper */
		return 1;
	}
	/* r->lower > r2->lower so r2 cannot in be in r */
	return 0;
}

int
range_issingle(struct range *r)
{
	return r->lower == r->upper-1;
}

long
range_as_constant(struct range *r)
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
