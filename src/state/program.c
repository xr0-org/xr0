#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
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
	int index;
	struct lexememarker *loc;
};

static enum program_state
program_state_init(struct ast_block *);

struct program *
program_create(struct ast_block *b)
{
	struct program *p = malloc(sizeof(struct program));
	p->b = b;
	p->s = program_state_init(b);
	p->index = 0;
	p->loc = NULL;
	return p;
}

void
program_setatend(struct program *p)
{
	p->s = PROGRAM_COUNTER_ATEND;
}

void
program_storeloc(struct program *p)
{
	if (ast_block_nstmts(p->b) > 0) {
		p->loc = ast_stmt_lexememarker(ast_block_stmts(p->b)[p->index]);
	}
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
program_destroy(struct program *p)
{
	/* no ownership of block */
	free(p);
}

struct program *
program_copy(struct program *old)
{
	struct program *new = program_create(old->b);
	new->s = old->s;
	new->index = old->index;
	return new;
}

char *
program_str(struct program *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s\n", ast_block_str(p->b, 1));
	return strbuilder_build(b);
}

int
program_index(struct program *p)
{
	return p->index;
}

char *
program_render(struct program *p)
{
	struct strbuilder *b = strbuilder_create();
	switch (p->s) {
	case PROGRAM_COUNTER_DECLS:
		strbuilder_printf(b, "%s", ast_block_render(p->b, p->index, true));
		break;
	case PROGRAM_COUNTER_STMTS:
		strbuilder_printf(b, "%s", ast_block_render(p->b, p->index, false));
		break;
	case PROGRAM_COUNTER_ATEND:
		strbuilder_printf(b, "\t<end of frame>");
		break;
	default:
		assert(false);
	}
	return strbuilder_build(b);
}

bool
program_atend(struct program *p)
{
	return p->s == PROGRAM_COUNTER_ATEND;
}

struct ast_expr *
program_prevcall(struct program *p)
{
	assert(p->s == PROGRAM_COUNTER_STMTS);
	struct ast_stmt *c = ast_block_stmts(p->b)[p->index-1];
	return ast_expr_copy(ast_stmt_register_call(c));
}

static void
program_nextdecl(struct program *p)
{
	assert(p->s == PROGRAM_COUNTER_DECLS);

	if (++p->index >= ast_block_ndecls(p->b)) {
		p->s = ast_block_nstmts(p->b)
			? PROGRAM_COUNTER_STMTS
			: PROGRAM_COUNTER_ATEND;
		p->index = 0;
	}
}

static bool
program_stmt_atend(struct program *, struct state *);

static void
program_nextstmt(struct program *p, struct state *s)
{
	assert(p->s == PROGRAM_COUNTER_STMTS);

	++p->index;
	if (program_stmt_atend(p, s)) {
		p->s = PROGRAM_COUNTER_ATEND;
	}
}

static bool
program_stmt_atend(struct program *p, struct state *s)
{
	return p->index >= ast_block_nstmts(p->b);
}

static struct error *
program_stmt_step(struct program *, struct state *);

struct error *
program_step(struct program *p, struct state *s)
{
	switch (p->s) {
	case PROGRAM_COUNTER_DECLS:
		state_declare(s, ast_block_decls(p->b)[p->index], false);
		program_nextdecl(p);
		return NULL;
	case PROGRAM_COUNTER_STMTS:
		return program_stmt_step(p, s);
	case PROGRAM_COUNTER_ATEND:
		if (state_frameid(s) != 0) {
			state_popframe(s);
		}
		return NULL;
	default:
		assert(false);
	}
}

static struct error *
program_stmt_process(struct program *p, struct state *s);

static struct error *
program_stmt_step(struct program *p, struct state *s)
{
	struct error *err = program_stmt_process(p, s);
	if (!err) {
		program_nextstmt(p, s);
		return NULL;
	}
	struct error *return_err = error_to_return(err);
	if (return_err) {
		state_return(s);
		return NULL;
	}
	return err;
}

static struct error *
program_stmt_process(struct program *p, struct state *s)
{
	struct ast_stmt *stmt = ast_block_stmts(p->b)[p->index];
	if (!state_islinear(s) && ast_stmt_linearisable(stmt)) {
		return ast_stmt_linearise(stmt, s);
	}
	switch (state_execmode(s)) {
	case EXEC_ABSTRACT:
		return ast_stmt_absprocess(stmt, s);
	case EXEC_ABSTRACT_NO_SETUP:
	case EXEC_SETUP:
		return ast_stmt_absprocess_nosetup(stmt, s);
	case EXEC_ACTUAL:
		return ast_stmt_process(stmt, s);
	case EXEC_VERIFY:
		return ast_stmt_verify(stmt, state_copy(s));
	default:
		assert(false);
	}
}

static struct error *
program_stmt_next(struct program *, struct state *);

struct error *
program_next(struct program *p, struct state *s)
{
	switch (p->s) {
	case PROGRAM_COUNTER_DECLS:
	case PROGRAM_COUNTER_ATEND:
		return program_step(p, s);
	case PROGRAM_COUNTER_STMTS:
		return program_stmt_next(p, s);	
	default:
		assert(false);
	}
}

static struct error *
program_stmt_next(struct program *p, struct state *s)
{
	int og_frame = state_frameid(s);
	do {
		struct error *err = state_step(s);
		if (err) {
			return err;
		}
	} while (state_frameid(s) != og_frame);

	return NULL;
}

char *
program_loc(struct program *p)
{
	if (p->loc) {
		return lexememarker_str(p->loc);
	}
	switch (p->s) {
	case PROGRAM_COUNTER_STMTS:
		return lexememarker_str(
			ast_stmt_lexememarker(ast_block_stmts(p->b)[p->index])
		);
	case PROGRAM_COUNTER_ATEND:
		assert(p->index && ast_block_stmts(p->b)); /* i.e., it's nonzero */
		return lexememarker_str(
			ast_stmt_lexememarker(ast_block_stmts(p->b)[p->index-1])
		);
	default:
		assert(false);
	}
}
