#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "util.h"
#include "text.h"

#include "intern.h"

struct text_pc {
	int i;
	struct text_pc *next;
};

struct text_pc *
text_pc_create(void)
{
	struct text_pc *pc = calloc(1, sizeof(struct text_pc)); 
	assert(pc);
	pc->i = 0;
	return pc;
}

struct text_pc *
text_pc_copy(struct text_pc *old)
{
	struct text_pc *new = text_pc_create();
	new->i = old->i;
	new->next = old->next ? text_pc_copy(old->next) : NULL;
	return new;
}

void
text_pc_destroy(struct text_pc *pc)
{
	if (pc->next)
		text_pc_destroy(pc->next);

	free(pc);
}

char *
text_pc_str(struct text_pc *pc)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%d", pc->i);
	if (pc->next) {
		char *next = text_pc_str(pc->next);
		strbuilder_printf(b, "->%s", next);
		free(next);
	}
	return strbuilder_build(b);
}

char *
text_pc_rendertop(struct text_pc *pc, struct text *t)
{
	return pc->next
		? text_pc_rendertop(pc->next, text_getnext(t, pc->i))
		: text_render(t, pc->i);
}

void
text_pc_advance(struct text_pc *pc, struct text *t)
{
	if (pc->next)
		text_pc_advance(pc->next, text_getnext(t, pc->i));
	else {
		pc->i++;
		assert(text_has(t, pc->i) || text_atend(t, pc->i));
	}
}

void
text_pc_enter(struct text_pc *pc, struct text *t, struct ast_block *b)
{
	if (pc->next)
		text_pc_enter(pc->next, text_getnext(t, pc->i), b);
	else {
		text_enter(t, pc->i, b);

		pc->next = text_pc_create();

		/* will trigger assertions if we've invalidated something */
		text_pc_getstmt(pc, t);
	}
}

struct ast_stmt *
text_pc_getstmt(struct text_pc *pc, struct text *t)
{
	return pc->next
		? text_pc_getstmt(pc->next, text_getnext(t, pc->i))
		: text_getstmt(t, pc->i);
}

struct ast_stmt *
text_pc_prevstmt(struct text_pc *pc, struct text *t)
{
	return pc->next
		? text_pc_prevstmt(pc->next, text_getnext(t, pc->i))
		: text_getstmt(t, pc->i-1);
}

int
text_pc_atblockend(struct text_pc *pc, struct text *t)
{
	return pc->next
		? text_pc_atblockend(pc->next, text_getnext(t, pc->i))
		: text_atend(t, pc->i);
}
