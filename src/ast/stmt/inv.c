#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "util.h"
#include "state.h"

#include "inv.h"

struct inv {
	char *l;
	struct ast_block *b;
};

static struct inv *
_create(char *l, struct ast_block *b)
{
	struct inv *inv = malloc(sizeof(struct inv));
	assert(inv);
	inv->l = l;
	inv->b = b;
	return inv;
}

struct inv *
inv_create(struct ast_block *b) { return _create(NULL, b); }

struct inv *
inv_copy(struct inv *inv)
{
	return _create(
		inv->l ? dynamic_str(inv->l): NULL,
		ast_block_copy(inv->b)
	);
}

void
inv_destroy(struct inv *inv)
{
	if (inv->l)
		free(inv->l);

	ast_block_destroy(inv->b);

	free(inv);
}

char *
inv_str(struct inv *inv, int indent_level)
{
	struct strbuilder *b = strbuilder_create();

	strbuilder_printf(b, "~ ");

	if (inv->l)
		strbuilder_printf(b, "%s: ", inv->l);

	char *s = ast_block_absstr(inv->b, indent_level);
	strbuilder_printf(b, "%s", s);
	free(s);

	return strbuilder_build(b);
}

void
inv_addlabel(struct inv *inv, char *l)
{
	assert(!inv->l);
	inv->l = l;
}

struct error *
inv_exec(struct inv *inv, struct state *s)
{
	state_pushinvariantframe(s, inv->b);
	return inv->l
		? error_enterinvariant(inv->l)
		: error_enterinvariant_nolabel();
}
