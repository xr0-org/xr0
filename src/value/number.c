#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "state.h"
#include "value.h"
#include "verifier.h"

#include "intern.h"
#include "number.h"
#include "range.h"
#include "_limits.h"

struct number {
	enum number_type {
		NUMBER_RANGES,
		NUMBER_EXPR,
	} type;
	union {
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
number_const_create(long l)
{
	return number_range_create(range_create(l, l+1));
}

static struct number *
_number_ranges_create(struct range_arr *ranges)
{
	struct number *num = _number_create(NUMBER_RANGES);
	num->ranges = ranges;
	return num;
}

struct number *
number_range_create(struct range *r)
{
	struct range_arr *arr = range_arr_create();
	range_arr_append(arr, r);
	return _number_ranges_create(arr);
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
	case NUMBER_RANGES:
		return _number_ranges_create(range_arr_copy(num->ranges));
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
number_short_str(struct number *n)
{
	if (number_isexpr(n)) {
		return ast_expr_str(number_as_expr(n));
	}
	return range_short_str(number_as_range(n));
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

static struct number *
_eval(struct ast_expr *, struct state *);

int
number_le(struct number *lhs, struct number *rhs, struct state *s)
{
	if (number_isexpr(lhs)) {
		return number_le(_eval(number_as_expr(lhs), s), rhs, s);
	}
	if (number_isexpr(rhs)) {
		return number_le(lhs, _eval(number_as_expr(rhs), s), s);
	}
	return number_as_const(lhs) <= number_as_const(rhs);
}

static struct number *
_eval(struct ast_expr *e, struct state *s)
{
	return value_as_number(
		tagval_value(tagval_res_as_tagval(ast_expr_rangeeval(e, s)))
	);
}

int
number_ge(struct number *lhs, struct number *rhs, struct state *s)
{
	return number_le(rhs, lhs, s);
}

int
number_eq(struct number *n, struct number *n0, struct state *s)
{
	return number_le(n, n0, s) && number_ge(n, n0, s);
}

int
number_lt(struct number *lhs, struct number *rhs, struct state *s)
{
	return number_le(lhs, rhs, s) && !number_eq(lhs, rhs, s);
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
_ranges_bound(struct number *, int islw);

static struct number *
_rconst_bound(struct ast_expr *, int islw, struct state *);

static struct number *
_number_bound(struct number *n, int islw, struct state *s)
{
	switch (n->type) {
	case NUMBER_RANGES:
		return _ranges_bound(n, islw);
	case NUMBER_EXPR:
		return _rconst_bound(n->expr, islw, s);
	default:
		assert(false);
	}
}

static struct number *
_ranges_bound(struct number *n, int islw)
{
	assert(n->type == NUMBER_RANGES);
	assert(range_arr_n(n->ranges) == 1);

	struct range *r = range_arr_range(n->ranges)[0];
	return number_const_create(islw ? range_lower(r) : range_upper(r));
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
	assert(number_isexpr(n));
	assert(number_issinglerange(n, s));
	assert(number_le(number_lw(n, s), number_lw(range, s), s));
	assert(number_le(number_up(range, s), number_up(n, s), s));

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

struct number *
number_ne_create(long val)
{
	return _number_ranges_create(range_arr_ne_create(val));
}

int
number_assume(struct number *n, struct number *split, struct state *s)
{
	assert(n->type == NUMBER_RANGES && split->type == NUMBER_RANGES);

	if (!range_arr_containsrangearr(n->ranges, split->ranges)) {
		return 0;
	}

	n->ranges = split->ranges;

	return 1;
}

long
number_as_const(struct number *n)
{
	assert(number_isconst(n));
	return range_as_const(range_arr_range(n->ranges)[0]);
}

int
number_isconst(struct number *n)
{
	switch (n->type) {
	case NUMBER_RANGES:
		return range_arr_n(n->ranges) == 1 &&
			range_issingle(range_arr_range(n->ranges)[0]);
	default:
		assert(false);
	}
}

struct range *
number_as_range(struct number *n)
{
	assert(n->type == NUMBER_RANGES && range_arr_n(n->ranges) == 1);
	return range_arr_range(n->ranges)[0];
}

static struct ast_expr *
_ranges_to_expr(struct range_arr *);

struct ast_expr *
number_to_expr(struct number *n)
{
	switch (n->type) {
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

	return ast_expr_constant_create(range_lower(r));
}

int
numbers_aresinglerange(struct number *lw, struct number *up)
{
	struct range *r = range_create(number_as_const(lw), number_as_const(up));
	int issingle = !number_isexpr(lw)
		&& !number_isexpr(up)
		&& range_issingle(r);
	range_destroy(r);
	return issingle;
}
