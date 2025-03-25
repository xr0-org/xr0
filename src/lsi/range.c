#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "range.h"

struct lsi_range { int lw, up; };

struct lsi_range *
lsi_range_create(int lw, int up)
{
	assert(lw <= up);

	struct lsi_range *r = malloc(sizeof(struct lsi_range));
	r->lw = lw;
	r->up = up;
	return r;
}

void
lsi_range_destroy(struct lsi_range *r)
{
	free(r);
}

char *
lsi_range_str(struct lsi_range *r)
{
	struct strbuilder *b = strbuilder_create();
	/* not represented as [lw?up] to avoid implying upper exclusivity */
	strbuilder_printf(b, "{%d %d}", r->lw, r->up);
	return strbuilder_build(b);
}

int
lsi_range_isconst(struct lsi_range *r)
{
	return r->lw == r->up;
}

int
lsi_range_as_const(struct lsi_range *r)
{
	assert(lsi_range_isconst(r));
	return r->lw;
}

struct lsi_le *
lsi_range_expr_le_lw(struct lsi_range *r, struct lsi_expr *e)
{
	return lsi_le_create(lsi_expr_copy(e), lsi_expr_const_create(r->lw));
}
