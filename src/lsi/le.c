#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "expr.h"

struct le { struct expr *l, *r; };

struct le *
le_create(struct expr *l, struct expr *r)
{
	struct le *le = malloc(sizeof(struct le));
	assert(le);
	le->l = l;
	le->r = r;
	return le;
}

struct le *
le_copy(struct le *old)
{
	return le_create(expr_copy(old->l), expr_copy(old->r));
}

void
le_destroy(struct le *le)
{
	expr_destroy(le->l);
	expr_destroy(le->r);
	free(le);
}

char *
le_str(struct le *le)
{
	struct strbuilder *b = strbuilder_create();
	char *l = expr_str(le->l),
	     *r = expr_str(le->r);
	strbuilder_printf(b, "%s <= %s", l, r);
	free(r);
	free(l);
	return strbuilder_build(b);
}
