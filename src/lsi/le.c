#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "expr.h"
#include "tally.h"

struct lsi_le {
	/* we represent l <= r as r-l */
	struct lsi_expr *_;
};

static struct lsi_le *
_lsi_le_create(struct lsi_expr *e)
{
	struct lsi_le *le = malloc(sizeof(struct lsi_le));
	assert(le);
	le->_ = e;
	return le;
}

struct lsi_le *
lsi_le_create(struct lsi_expr *l, struct lsi_expr *r)
{
	return _lsi_le_create(
		/* r - l */
		lsi_expr_sum_create(
			r,
			lsi_expr_product_create(lsi_expr_const_create(1), l)
		)
	);
}

struct lsi_le *
_lsi_le_copy(struct lsi_le *old)
{
	return _lsi_le_create(_lsi_expr_copy(old->_));
}

void
_lsi_le_destroy(struct lsi_le *le)
{
	_lsi_expr_destroy(le->_);
	free(le);
}

static struct lsi_expr *
_lhs(struct lsi_le *);

static struct lsi_expr *
_rhs(struct lsi_le *);

char *
lsi_le_str(struct lsi_le *le)
{
	struct strbuilder *b = strbuilder_create();

	struct lsi_expr *lhs = _lhs(le),
			*rhs = _rhs(le);

	char *l = lsi_expr_str(lhs),
	     *r = lsi_expr_str(rhs);
	strbuilder_printf(b, "%s <= %s", l, r);
	free(r);
	free(l);

	_lsi_expr_destroy(rhs);
	_lsi_expr_destroy(lhs);

	return strbuilder_build(b);
}

static struct lsi_expr *
_lhs(struct lsi_le *le)
{
	struct tally *t = tally_create();

	/* le is expressed as r-l, so we add the negative terms */
	struct tally *t_le = _lsi_expr_tally(le->_);
	struct string_arr *arr = tally_getvars(t_le);
	for (int i = 0; i < string_arr_n(arr); i++) {
		char *var = string_arr_s(arr)[i];
		int coef = tally_getcoef(t, var);
		if (coef > 0) {
			tally_setcoef(t, dynamic_str(var), -coef);
		}
	}
	string_arr_destroy(arr);
	int c = tally_getconst(t_le);
	if (c < 0) {
		tally_setconst(t, -c);
	}

	return _lsi_expr_tally_create(t);
}

static struct lsi_expr *
_rhs(struct lsi_le *le)
{
	struct tally *t = tally_create();

	/* le is expressed as r-l, so we add the positive terms */
	struct tally *t_le = _lsi_expr_tally(le->_);
	struct string_arr *arr = tally_getvars(t_le);
	for (int i = 0; i < string_arr_n(arr); i++) {
		char *var = string_arr_s(arr)[i];
		int coef = tally_getcoef(t, var);
		if (coef > 0) {
			tally_setcoef(t, dynamic_str(var), coef);
		}
	}
	string_arr_destroy(arr);
	int c = tally_getconst(t_le);
	if (c > 0) {
		tally_setconst(t, c);
	}

	return _lsi_expr_tally_create(t);
}


struct tally *
_lsi_le_tally(struct lsi_le *le)
{
	return _lsi_expr_tally(le->_);
}
