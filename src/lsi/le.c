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
			lsi_expr_product_create(lsi_expr_const_create(-1), l)
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

	/* l <= r is expressed as r-l */
	struct lsi_expr *lhs = _lsi_expr_negatives(le->_),
			*rhs = _lsi_expr_positives(le->_);

	char *l = lsi_expr_str(lhs),
	     *r = lsi_expr_str(rhs);
	strbuilder_printf(b, "%s <= %s", l, r);
	free(r);
	free(l);

	_lsi_expr_destroy(rhs);
	_lsi_expr_destroy(lhs);

	return strbuilder_build(b);
}
