#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "util.h"

#include "intern.h"

struct text {
	struct ast_block *b;
	struct text **t;
};

struct text *
text_create(struct ast_block *b)
{
	struct text *t = malloc(sizeof(struct text));
	assert(t);
	t->b = b;
	t->t = malloc(sizeof(struct text *) * ast_block_nstmts(b));
	return t;
}

void
text_destroy(struct text *t)
{
	int i;

	for (i = 0; i < ast_block_nstmts(t->b); i++)
		text_destroy(t->t[i]);

	ast_block_destroy(t->b);

	free(t);
}

struct text *
text_getnext(struct text *t, int i)
{
	assert(text_has(t, i));

	a_printf(t->t[i], "need to create next if compound stmt\n");
	return t->t[i];
}

void
text_putgen(struct text *t, int i, struct ast_block *gen)
{
	assert(text_has(t, i));

	assert(!t->t[i]);

	t->t[i] = text_create(gen);
}

struct ast_stmt *
text_getstmt(struct text *t, int i)
{
	assert(text_has(t, i));

	return ast_block_stmts(t->b)[i];
}

int
text_has(struct text *t, int i)
{
	int n = ast_block_nstmts(t->b);
	return 0 <= i && i < n;
}

int
text_atend(struct text *t, int i)
{
	int n = ast_block_nstmts(t->b);
	assert(0 <= i && i <= n);

	return i == n;
}
