#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"
#include "_limits.h"

#include "tally.h"

struct lsi_expr { struct tally *_; };

struct lsi_expr *
_lsi_expr_tally_create(struct tally *t)
{
	struct lsi_expr *e = malloc(sizeof(struct lsi_expr));
	assert(e);
	e->_ = t;
	return e;
}

struct lsi_expr *
lsi_expr_const_create(int c)
{
	a_printf(
		C89_INT_MIN <= c && c <= C89_INT_MAX,
		"only values between INT_MIN and INT_MAX are supported: "\
		"have %d\n", c
	);

	struct lsi_expr *e = _lsi_expr_tally_create(tally_create());
	tally_setconst(e->_, c);
	return e;
}

struct lsi_expr *
lsi_expr_var_create(char *s)
{
	struct lsi_expr *e = _lsi_expr_tally_create(tally_create());
	tally_setcoef(e->_, s, 1);
	return e;
}

struct lsi_expr *
lsi_expr_sum_create(struct lsi_expr *e0, struct lsi_expr *e1)
{
	return _lsi_expr_tally_create(tally_sum(e0->_, e1->_));
}

static int
_isconst(struct lsi_expr *);

static int
_as_const(struct lsi_expr *);

struct lsi_expr *
lsi_expr_product_create(struct lsi_expr *e0, struct lsi_expr *e1)
{
	if (_isconst(e1)) {
		if (_isconst(e0)) {
			return lsi_expr_const_create(
				tally_getconst(e0->_)*tally_getconst(e1->_)
			);
		}
		return lsi_expr_product_create(e1, e0);
	}
	a_printf(_isconst(e0), "one operand must be constant\n");

	return _lsi_expr_tally_create(tally_product(e1->_, _as_const(e0)));
}

static int
_isconst(struct lsi_expr *e)
{
	struct string_arr *arr = tally_getvars(e->_);
	int isconst = string_arr_n(arr) == 0;
	string_arr_destroy(arr);
	return isconst;
}

static int
_as_const(struct lsi_expr *e)
{
	assert(_isconst(e));
	return tally_getconst(e->_);
}

struct lsi_expr *
_lsi_expr_copy(struct lsi_expr *old)
{
	return _lsi_expr_tally_create(tally_copy(old->_));
}

void
_lsi_expr_destroy(struct lsi_expr *e)
{
	tally_destroy(e->_);
	free(e);
}

char *
lsi_expr_str(struct lsi_expr *e)
{
	int i;
	struct strbuilder *b = strbuilder_create();

	struct string_arr *arr = tally_getvars(e->_); 
	int len = string_arr_n(arr);
	for (i = 0; i < len; i++) {
		char *v = string_arr_s(arr)[i];
		int coef = tally_getcoef(e->_, v);
		strbuilder_printf(b, "%d*%s%s", coef, v, i+1<len ? " " : "");
	}
	string_arr_destroy(arr);
	strbuilder_printf(b, "%s%d", len>0 ? "+" : "", tally_getconst(e->_));

	return strbuilder_build(b);
}

struct tally *
_lsi_expr_tally(struct lsi_expr *e)
{
	return e->_;
}

struct lsi_expr *
_lsi_expr_positives(struct lsi_expr *e)
{
	struct tally *t = tally_create();

	struct string_arr *arr = tally_getvars(e->_);
	for (int i = 0; i < string_arr_n(arr); i++) {
		char *var = string_arr_s(arr)[i];
		int coef = tally_getcoef(e->_, var);
		if (coef > 0) {
			tally_setcoef(t, dynamic_str(var), coef);
		}
	}
	string_arr_destroy(arr);
	int c = tally_getconst(e->_);
	if (c > 0) {
		tally_setconst(t, c);
	}

	return _lsi_expr_tally_create(t);
}

struct lsi_expr *
_lsi_expr_negatives(struct lsi_expr *e)
{
	struct tally *t = tally_create();

	struct string_arr *arr = tally_getvars(e->_);
	for (int i = 0; i < string_arr_n(arr); i++) {
		char *var = string_arr_s(arr)[i];
		int coef = tally_getcoef(e->_, var);
		if (coef < 0) {
			tally_setcoef(t, dynamic_str(var), -coef);
		}
	}
	string_arr_destroy(arr);
	int c = tally_getconst(e->_);
	if (c < 0) {
		tally_setconst(t, -c);
	}

	return _lsi_expr_tally_create(t);
}
