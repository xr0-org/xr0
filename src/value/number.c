#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>

#include "ast.h"
#include "state.h"
#include "value.h"

#include "cconst.h"
#include "intern.h"
#include "number.h"
#include "range.h"

struct number {
	enum number_type {
		NUMBER_CCONST,
		NUMBER_RANGES,
		NUMBER_EXPR,
	} type;
	union {
		struct cconst *cconst;
		struct range_arr *ranges;
		struct ast_expr *expr;
	};
};

static struct number *
_number_create(enum number_type type)
{
	struct number *num = malloc(sizeof(struct number));
	assert(num);
	num->type = type;
	return num;
}

struct number *
number_cconst_create(struct cconst *c)
{
	struct number *num = _number_create(NUMBER_CCONST);
	num->cconst = c;
	return num;
}

struct number *
number_single_create(int val)
{
	return number_cconst_create(cconst_constant_create(val));
}

struct number *
number_ranges_create(struct range_arr *ranges)
{
	struct number *num = _number_create(NUMBER_RANGES);
	num->ranges = ranges;
	return num;
}

struct number *
number_expr_create(struct ast_expr *e)
{
	struct number *num = _number_create(NUMBER_EXPR);
	num->expr = e;
	return num;
}

struct number *
number_copy(struct number *num)
{
	switch (num->type) {
	case NUMBER_CCONST:
		return number_cconst_create(cconst_copy(num->cconst));
	case NUMBER_RANGES:
		return number_ranges_create(range_arr_copy(num->ranges));
	case NUMBER_EXPR:
		return number_expr_create(ast_expr_copy(num->expr));
	default:
		assert(false);
	}
}

void
number_destroy(struct number *n)
{
	switch (n->type) {
	case NUMBER_CCONST:
		cconst_destroy(n->cconst);
		break;
	case NUMBER_RANGES:
		range_arr_destroy(n->ranges);
		break;
	case NUMBER_EXPR:
		ast_expr_destroy(n->expr);
		break;
	default:
		assert(false);
	}
	free(n);
}

static char *
_ranges_str(struct number *);

char *
number_str(struct number *num)
{
	switch (num->type) {
	case NUMBER_CCONST:
		return cconst_str(number_as_cconst(num));
	case NUMBER_RANGES:
		return _ranges_str(num);
	case NUMBER_EXPR:
		return ast_expr_str(number_as_expr(num));
	default:
		assert(false);
	}
}

static char *
_ranges_str(struct number *num)
{
	assert(num->type == NUMBER_RANGES);

	struct strbuilder *b = strbuilder_create();
	int n = range_arr_n(num->ranges);
	struct range **range = range_arr_range(num->ranges);
	strbuilder_putc(b, '{');
	for (int i = 0; i < n; i++) {
		char *r = range_str(range[i]);
		strbuilder_printf(b, "%s%s", r, (i+1 < n ? ", " : ""));
		free(r);
	}
	strbuilder_putc(b, '}');
	return strbuilder_build(b);
}

char *
number_str_inrange(struct number *n)
{
	return cconst_str_inrange(number_as_cconst(n));
}

int
number_iscconst(struct number *n)
{
	return n->type == NUMBER_CCONST;
}

struct cconst *
number_as_cconst(struct number *n)
{
	if (number_iscconst(n)) {
		return n->cconst;
	}
	assert(number_isrange(n));
	struct range_arr *arr = n->ranges;
	assert(range_arr_n(arr) == 1);
	return range_as_cconst(range_arr_range(arr)[0]);
}

int
number_isrange(struct number *n)
{
	return n->type == NUMBER_RANGES;
}

int
number_isexpr(struct number *n)
{
	return n->type == NUMBER_EXPR;
}

struct ast_expr *
number_as_expr(struct number *n)
{
	assert(number_isexpr(n));
	return n->expr;
}

int
number_eq(struct number *n, struct number *n0)
{
	return cconst_eq(number_as_cconst(n), number_as_cconst(n0));
}

int
number_lt(struct number *lhs, struct number *rhs)
{
	return cconst_lt(number_as_cconst(lhs), number_as_cconst(rhs));
}

int
number_le(struct number *lhs, struct number *rhs)
{
	return cconst_le(number_as_cconst(lhs), number_as_cconst(rhs));
}

int
number_ge(struct number *lhs, struct number *rhs)
{
	return cconst_ge(number_as_cconst(lhs), number_as_cconst(rhs));
}

static struct number *
_number_bound(struct number *n, int islw, struct state *s);

struct number *
number_lw(struct number *n, struct state *s)
{
	return _number_bound(n, 1, s);
}

struct number *
number_up(struct number *n, struct state *s)
{
	return _number_bound(n, 0, s);
}

static struct number *
_cconst_bound(struct cconst *, int islw);

static struct number *
_ranges_bound(struct number *, int islw);

static struct number *
_rconst_bound(struct ast_expr *, int islw, struct state *);

static struct number *
_number_bound(struct number *n, int islw, struct state *s)
{
	switch (n->type) {
	case NUMBER_CCONST:
		return _cconst_bound(n->cconst, islw);
	case NUMBER_RANGES:
		return _ranges_bound(n, islw);
	case NUMBER_EXPR:
		return _rconst_bound(n->expr, islw, s);
	default:
		assert(false);
	}
}

static struct number *
_cconst_bound(struct cconst *c, int islw)
{
	if (islw) {
		return number_cconst_create(cconst_copy(c));
	}
	/* exclude MIN/MAX cases for simplicity */
	assert(cconst_isconstant(c));
	int k = cconst_as_constant(c);
	assert(k < INT_MAX);
	return number_cconst_create(cconst_constant_create(k + 1));
}

static struct number *
_ranges_bound(struct number *n, int islw)
{
	assert(n->type == NUMBER_RANGES);
	assert(range_arr_n(n->ranges) == 1);

	struct range *r = range_arr_range(n->ranges)[0];
	return islw ? range_lower(r) : range_upper(r);
}


static struct number *
_rconst_bound(struct ast_expr *e, int islw, struct state *s)
{
	struct value *rconst = tagval_value(
		tagval_res_as_tagval(ast_expr_rangeeval(e, s))
	);
	assert(rconst);
	return _number_bound(value_as_number(rconst), islw, s);
}

static int
_rconst_issinglerange(struct ast_expr *, struct state *);

int
number_issinglerange(struct number *n, struct state *s)
{
	switch (n->type) {
	case NUMBER_CCONST:
		return 1;
	case NUMBER_RANGES:
		return range_arr_n(n->ranges) == 1;
	case NUMBER_EXPR:
		return _rconst_issinglerange(n->expr, s);
	default:
		assert(false);
	}
}

static int
_rconst_issinglerange(struct ast_expr *e, struct state *s)
{
	struct value *rconst = tagval_value(
		tagval_res_as_tagval(ast_expr_rangeeval(e, s))
	);
	assert(rconst);
	return number_issinglerange(value_as_number(rconst), s);
}


void
number_splitto(struct number *n, struct number *range, struct map *splits,
		struct state *s)
{
	assert(
		n->type == NUMBER_EXPR
		&& number_issinglerange(n, s)
		&& number_le(number_lw(n, s), number_lw(range, s))
		&& number_le(number_up(range, s), number_up(n, s))
	);

	map_set(
		splits,
		dynamic_str(ast_expr_as_identifier(n->expr)),
		range
	);
}

static struct number *
_rconst_tosinglerange(char *, struct state *);

struct number *
number_tosinglerange(struct number *n, struct state *s)
{
	switch (n->type) {
	case NUMBER_CCONST:
		return n;
	case NUMBER_RANGES:
		assert(number_issinglerange(n, s));
		return n;
	case NUMBER_EXPR:
		return _rconst_tosinglerange(
			ast_expr_as_identifier(n->expr), s
		);
	default:
		assert(false);
	}
}

static struct number *
_rconst_tosinglerange(char *id, struct state *s)
{
	struct value *rconst = state_getrconst(s, id);
	assert(rconst);
	return number_tosinglerange(value_as_number(rconst), s);
}


struct range_arr *
range_arr_ne_create(int val)
{
	struct range_arr *arr = range_arr_create();
	range_arr_append(
		arr, 
		range_create( /* [MIN:val) */
			number_cconst_create(cconst_min_create()),
			number_cconst_create(cconst_constant_create(val))
		)
	);
	range_arr_append(
		arr, 
		range_create( /* [val+1:MAX) XXX */
			number_cconst_create(cconst_constant_create(val+1)),
			number_cconst_create(cconst_max_create())
		)
	);
	return arr;
}

struct number *
number_ne_create(int val)
{
	return number_ranges_create(range_arr_ne_create(val));
}

struct number *
number_singlerange_create(struct number *lw, struct number *up)
{
	struct range_arr *arr = range_arr_create();
	range_arr_append(
		arr, 
		range_create(lw, up)
	);
	return number_ranges_create(arr);
}

bool
ranges_equal(struct number *n1, struct number *n2);

bool
range_equal(struct range *, struct range *);

bool
ranges_equal(struct number *n1, struct number *n2)
{
	assert(n1->type == n2->type && n1->type == NUMBER_RANGES);

	int len = range_arr_n(n1->ranges);
	if (len != range_arr_n(n2->ranges)) {
		return false;
	}

	struct range **n1_r = range_arr_range(n1->ranges),
			    **n2_r = range_arr_range(n2->ranges);
	for (int i = 0; i < len; i++) {
		if (!range_equal(n1_r[i], n2_r[i])) {
			return false;
		}
	}

	return true;
}

static struct number *
_cconst_to_range(struct cconst *);

int
number_assume(struct number *n, struct number *split)
{
	if (number_iscconst(split)) {
		split = _cconst_to_range(number_as_cconst(split));
	}

	assert(n->type == NUMBER_RANGES && split->type == NUMBER_RANGES);

	if (!range_arr_containsrangearr(n->ranges, split->ranges)) {
		return false;
	}

	n->ranges = split->ranges;

	return true;
}

static struct number *
_cconst_to_range(struct cconst *c)
{
	int k = cconst_as_constant(c);
	struct range_arr *r = range_arr_create();
	range_arr_append(
		r,
		range_create(
			number_cconst_create(cconst_constant_create(k)),
			number_cconst_create(cconst_constant_create(k+1))
		)
	);
	return number_ranges_create(r);
}


int
number_isconstant(struct number *n)
{
	switch (n->type) {
	case NUMBER_CCONST:
		return cconst_isconstant(n->cconst);
	case NUMBER_RANGES:
		return range_arr_n(n->ranges) == 1 &&
			range_issingle(range_arr_range(n->ranges)[0]);
	default:
		assert(false);
	}
}

static struct ast_expr *
_ranges_to_expr(struct range_arr *);

struct ast_expr *
number_to_expr(struct number *n)
{
	switch (n->type) {
	case NUMBER_CCONST:
		return ast_expr_constant_create(cconst_as_constant(n->cconst));
	case NUMBER_RANGES:
		return _ranges_to_expr(n->ranges);
	case NUMBER_EXPR:
		return ast_expr_copy(n->expr);
	default:
		assert(false);
	}
}

static struct ast_expr *
_ranges_to_expr(struct range_arr *arr)
{
	assert(range_arr_n(arr) == 1);
	struct range *r = range_arr_range(arr)[0];
	assert(range_issingle(r));

	return ast_expr_constant_create(
		cconst_as_constant(number_as_cconst(range_lower(r)))
	);
}

int
numbers_aresinglerange(struct number *lw, struct number *up)
{
	return cconsts_aresinglerange(
		number_as_cconst(lw), number_as_cconst(up)
	);
}
