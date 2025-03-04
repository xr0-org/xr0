#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "util.h"
#include "lsi.h"

#include "le.h"
#include "expr.h"
#include "tally.h"

struct lsi_le {
	/* we represent l <= r as l - r */
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
		/* l - r */
		lsi_expr_sum_create(
			l,
			lsi_expr_product_create(lsi_expr_const_create(-1), r)
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

char *
lsi_le_str(struct lsi_le *le)
{
	struct strbuilder *b = strbuilder_create();

	/* because l <= r is represented as l - r, moving the constant term to
	 * the rhs requires negating them */

	int _const = -_lsi_expr_constterm(le->_);

	struct lsi_expr *var = _lsi_expr_varterms(le->_);

	char *s = lsi_expr_str(var);
	strbuilder_printf(b, "%s <= %d", s, _const);
	free(s);

	_lsi_expr_destroy(var);

	return strbuilder_build(b);
}

struct string_arr *
_lsi_le_getvars(struct lsi_le *le)
{
	return _lsi_expr_getvars(le->_);
}

long
_lsi_le_getstdformcoef(struct lsi_le *le, char *var)
{
	return _lsi_expr_getcoef(le->_, var);
}

struct lsi_expr *
_lsi_le_lowerbound(struct lsi_le *le, char *var)
{
	long coef = _lsi_le_getstdformcoef(le, var);
	assert(coef < 0);
	assert(coef == -1); /* TODO: fractions */

	/* l <= r stored as l - r, so the excluding expression is solved lhs */
	return _lsi_expr_except(le->_, var);
}

struct lsi_expr *
_lsi_le_upperbound(struct lsi_le *le, char *var)
{
	long coef = _lsi_le_getstdformcoef(le, var);
	assert(coef > 0);
	assert(coef == 1); /* TODO: fractions */

	/* l <= r stored as l - r, so the excluding expression must be negated
	 * to move it to the rhs */
	return lsi_expr_product_create(
		lsi_expr_const_create(-1), _lsi_expr_except(le->_, var)
	);
}

int
_lsi_le_isfeasible(struct lsi_le *le)
{
	int i;

	struct string_arr *vars = _lsi_le_getvars(le);
	for (i = 0; i < string_arr_n(vars); i++)
		assert(!_lsi_expr_getcoef(le->_, string_arr_s(vars)[i]));
	string_arr_destroy(vars);

	/* l <= r stored as l - r, so the (constant) inequality is only feasible
	 * if the expression is <= 0 */
	return _lsi_expr_constterm(le->_) <= 0;
}
