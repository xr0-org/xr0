#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "program.h"
#include "state.h"
#include "util.h"

struct program {
	struct ast_block *b;
	enum program_state {
		PROGRAM_COUNTER_DECLS,
		PROGRAM_COUNTER_STMTS,
		PROGRAM_COUNTER_ATEND,
	} s;
	char *name;
	int index;
};

static enum program_state
program_state_init(struct ast_block *);

struct program *
program_create(struct ast_block *b, char *name)
{
	struct program *pc = malloc(sizeof(struct program));
	pc->b = b;
	pc->s = program_state_init(b);
	pc->index = 0;
	pc->name = dynamic_str(name);
	return pc;
}

static enum program_state
program_state_init(struct ast_block *b)
{
	return ast_block_ndecls(b)
		? PROGRAM_COUNTER_DECLS
		: ast_block_nstmts(b)
		? PROGRAM_COUNTER_STMTS
		: PROGRAM_COUNTER_ATEND;
}

void
program_destroy(struct program *pc)
{
	/* no ownership of block */
	free(pc->name);
	free(pc);
}

struct program *
program_copy(struct program *old)
{
	struct program *new = program_create(old->b, old->name);
	new->s = old->s;
	new->index = old->index;
	return new;
}

char *
program_name(struct program *pc)
{
	return pc->name;
}

void
program_changename(struct program *pc, char *new_name)
{
	free(pc->name);
	pc->name = dynamic_str(new_name);
}

bool
program_atend(struct program *pc)
{
	return pc->s == PROGRAM_COUNTER_ATEND;
}

static void
program_nextdecl(struct program *pc)
{
	assert(pc->s == PROGRAM_COUNTER_DECLS);

	if (++pc->index >= ast_block_ndecls(pc->b)) {
		pc->s = ast_block_nstmts(pc->b)
			? PROGRAM_COUNTER_STMTS
			: PROGRAM_COUNTER_ATEND;
		pc->index = 0;
	}
}

static bool
program_stmt_atend(struct program *, struct state *);

static void
program_nextstmt(struct program *pc, struct state *s)
{
	assert(pc->s == PROGRAM_COUNTER_STMTS);

	if (program_stmt_atend(pc, s)) {
		pc->s = PROGRAM_COUNTER_ATEND;
	}
}

static bool
program_stmt_atend(struct program *pc, struct state *s)
{
	return ast_stmt_isterminal(ast_block_stmts(pc->b)[pc->index], s)
		|| ++pc->index >= ast_block_nstmts(pc->b);
}


static struct error *
program_stmt_step(struct program *pc, bool abstract, 
		struct state *s);

struct error *
program_exec(struct program *pc, bool abstract, struct state *s)
{
	switch (pc->s) {
	case PROGRAM_COUNTER_DECLS:
		state_declare(s, ast_block_decls(pc->b)[pc->index], false);
		program_nextdecl(pc);
		return NULL;
	case PROGRAM_COUNTER_STMTS:
		return program_stmt_step(pc, abstract, s);
	case PROGRAM_COUNTER_ATEND:
	default:
		assert(false);
	}
}

static struct error *
program_stmt_process(struct program *pc, bool abstract, 
		struct state *s);

static struct error *
program_stmt_step(struct program *pc, bool abstract, 
		struct state *s)
{
	struct error *err = program_stmt_process(pc, abstract, s);
	if (err) {
		return err;
	}
	program_nextstmt(pc, s);
	return NULL;
}

static struct error *
program_stmt_process(struct program *pc, bool abstract, 
		struct state *s)
{
	struct ast_stmt *stmt = ast_block_stmts(pc->b)[pc->index];
	if (abstract) {
		if (ast_stmt_ispre(stmt)) {
			return NULL;
		}
		return ast_stmt_absprocess(stmt, pc->name, s, true);
	}
	return ast_stmt_process(stmt, pc->name, s);
}
