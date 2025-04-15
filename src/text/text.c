#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"

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
	assert(i < ast_block_nstmts(t->b));
	assert(t->t[i]);
	return t->t[i];
}

struct ast_stmt *
text_getstmt(struct text *t, int i)
{
	assert(i < ast_block_nstmts(t->b));
	return ast_block_stmts(t->b, i);
}

int
text_atend(struct text *t, int i)
{
	int n = ast_block_nstmts(t->b);
	assert(i <= n);
	return i == n;
}
