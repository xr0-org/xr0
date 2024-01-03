#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "props.h"
#include "util.h"

struct props {
	int n;
	struct ast_expr **prop;
};

struct props *
props_create()
{
	return calloc(1, sizeof(struct props));
}

struct props *
props_copy(struct props *old)
{
	struct props *new = props_create();
	for (int i = 0; i < old->n; i++) {
		props_install(new, ast_expr_copy(old->prop[i]));
	}
	return new;
}

void
props_destroy(struct props *p)
{
	for (int i = 0; i < p->n; i++) {
		ast_expr_destroy(p->prop[i]);
	}
	free(p);
}

char *
props_str(struct props *p, char *indent)
{
	if (p->n == 0) {
		return dynamic_str("");
	}
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s⊢ ", indent);
	for (int i = 0; i < p->n; i++) {
		char *e = ast_expr_str(p->prop[i]);
		strbuilder_printf(b, "%s%s", e, (i+1<p->n ? ", " : ""));
		free(e);
	}
	strbuilder_printf(b, "\n");
	return strbuilder_build(b);
}

void
props_install(struct props *p, struct ast_expr *e)
{
	p->prop = realloc(p->prop, sizeof(struct ast_expr *) * ++p->n);
	p->prop[p->n-1] = e;
}

bool
props_get(struct props *p, struct ast_expr *e)
{
	for (int i = 0; i < p->n; i++) {
		/* TODO: logical comparison */
		if (ast_expr_equal(e, p->prop[i])) {
			return true;
		}
	}
	return false;
}

bool
props_contradict(struct props *p)
{
	for (int i = 0; i < p->n; i++) {
		for (int j = 0; j < p->n; j++) {
			/* TODO: check for logical contradiction */
			struct ast_expr *p1 = p->prop[i],
					*p2 = p->prop[j];
			struct ast_expr *not_p1 = ast_expr_inverted_copy(p1, true),
					*not_p2 = ast_expr_inverted_copy(p2, true);
			bool contra = ast_expr_equal(p1, not_p2)
					|| ast_expr_equal(not_p1, p2);
			ast_expr_destroy(not_p2);
			ast_expr_destroy(not_p1);
			if (contra) {
				return contra;
			}
		}
	}
	return false;
}