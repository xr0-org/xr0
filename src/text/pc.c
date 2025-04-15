#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

struct text_pc {
	int i;
	struct text_pc *next;
};

struct text_pc *
text_pc_create(int i)
{
	struct text_pc *pc = calloc(1, sizeof(struct text_pc)); 
	assert(pc);
	pc->i = i;
	return pc;
}

void
text_pc_destroy(struct text_pc *pc)
{
	if (pc->next)
		text_pc_destroy(pc->next);

	free(pc);
}

void
text_pc_add(struct text_pc *pc, int i)
{
	if (pc->next)
		text_pc_add(pc->next, i);
	else
		pc->next = text_pc_create(i);
}

struct ast_stmt *
text_pc_getstmt(struct text_pc *pc, struct text *t)
{
	return pc->next
		? text_pc_getstmt(pc->next, text_getnext(t, pc->i))
		: text_getstmt(t, pc->i);
}
