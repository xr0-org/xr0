#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "expr.h"

struct lsi_le { struct lsi_expr *l, *r; };

struct lsi_le *
lsi_le_create(struct lsi_expr *l, struct lsi_expr *r)
{
	struct lsi_le *le = malloc(sizeof(struct lsi_le));
	assert(le);
	le->l = l;
	le->r = r;
	return le;
}

struct lsi_le *
_lsi_le_copy(struct lsi_le *old)
{
	return lsi_le_create(_lsi_expr_copy(old->l), _lsi_expr_copy(old->r));
}

void
_lsi_le_destroy(struct lsi_le *le)
{
	_lsi_expr_destroy(le->l);
	_lsi_expr_destroy(le->r);
	free(le);
}

char *
lsi_le_str(struct lsi_le *le)
{
	struct strbuilder *b = strbuilder_create();
	char *l = lsi_expr_str(le->l),
	     *r = lsi_expr_str(le->r);
	strbuilder_printf(b, "%s <= %s", l, r);
	free(r);
	free(l);
	return strbuilder_build(b);
}
