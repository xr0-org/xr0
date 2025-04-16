#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "breakpoint.h"
#include "lex.h"
#include "program.h"
#include "state.h"
#include "util.h"
#include "text.h"

struct program {
	struct text *t; /* not owned */
	struct text_pc *pc;
	enum program_state { STATE_STMTS, STATE_ATEND, } s;
	struct lexememarker *loc;
	enum execution_mode {
		MODE_FINDSETUP,
		MODE_SETUP,
		MODE_ABSTRACT,
		MODE_ACTUAL,
		MODE_VERIFY,
	} mode;
};

static struct program *
_create(struct text *t, struct text_pc *pc, enum execution_mode m)
{
	struct program *p = malloc(sizeof(struct program));
	assert(p);
	p->t = t;
	p->pc = pc;
	p->s = text_pc_atblockend(pc, t)
		? STATE_ATEND
		: STATE_STMTS;
	p->loc = NULL;
	p->mode = m;
	return p;
}

struct program *
program_abstract_create(struct ast_block *b)
{
	return _create(text_create(b), text_pc_create(), MODE_ABSTRACT);
}

struct program *
program_actual_create(struct ast_block *b)
{
	return _create(text_create(b), text_pc_create(), MODE_ACTUAL);
}

struct program *
program_setup_create(struct ast_block *b)
{
	return _create(text_create(b), text_pc_create(), MODE_SETUP);
}

struct program *
program_findsetup_create(struct ast_block *b)
{
	return _create(text_create(b), text_pc_create(), MODE_FINDSETUP);
}

struct program *
program_verify_create(struct ast_block *b)
{
	return _create(text_create(b), text_pc_create(), MODE_VERIFY);
}

struct program *
program_nestedblock_create(struct program *origin, struct ast_block *b)
{
	struct text_pc *pc = text_pc_copy(origin->pc);
	text_pc_enter(pc, origin->t, b);
	return _create(origin->t, pc, origin->mode);
}

struct program *
program_linear_create(struct program *origin, struct ast_block *gen)
{
	struct text_pc *pc = text_pc_copy(origin->pc);
	text_pc_enter(pc, origin->t, gen);
	return _create(origin->t, pc, origin->mode);
}

struct program *
program_copy(struct program *old)
{
	struct program *new = malloc(sizeof(struct program));
	assert(new);
	new->t = old->t;
	new->pc = text_pc_copy(old->pc);
	new->s = old->s;
	new->loc = old->loc ? lexememarker_copy(old->loc) : NULL;
	new->mode = old->mode;
	return new;
}

void
program_destroy(struct program *p)
{
	/* no ownership of text */
	text_pc_destroy(p->pc);
	free(p);
}

void
program_setatend(struct program *p)
{
	p->s = STATE_ATEND;
}

void
program_storeloc(struct program *p)
{
	if (!text_pc_atblockend(p->pc, p->t))
		p->loc = ast_stmt_lexememarker(text_pc_getstmt(p->pc, p->t));
}

int
program_modecanverify(struct program *p)
{
	return p->mode == MODE_VERIFY;
}

int
program_modecanrunxr0cmd(struct program *p)
{
	switch (p->mode) {
	case MODE_SETUP:
	case MODE_ABSTRACT:
		return 1;
	default:
		return 0;
	}
}

char *
program_render(struct program *p)
{
	switch (p->s) {
	case STATE_STMTS:
		return text_pc_rendertop(p->pc, p->t);
	case STATE_ATEND:
		return dynamic_str("\t<end of frame>\n");
	default:
		assert(false);
	}
}

bool
program_atend(struct program *p)
{
	return p->s == STATE_ATEND;
}

struct ast_expr *
program_prevcall(struct program *p)
{
	assert(p->s == STATE_STMTS);

	return ast_expr_copy(ast_stmt_asm_call(text_pc_prevstmt(p->pc, p->t)));
}

static void
program_nextstmt(struct program *p, struct state *s)
{
	assert(p->s == STATE_STMTS);

	text_pc_advance(p->pc, p->t);

	if (text_pc_atblockend(p->pc, p->t))
		p->s = STATE_ATEND;
}

static struct error *
program_stmt_step(struct program *, struct state *);

struct error *
program_step(struct program *p, struct state *s)
{
	switch (p->s) {
	case STATE_STMTS:
		return program_stmt_step(p, s);
	case STATE_ATEND:
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
	struct error *break_err = error_to_break(err);
	if (break_err) {
		state_break(s);
		return NULL;
	}
	return err;
}

static struct error *
program_stmt_process(struct program *p, struct state *s)
{
	struct ast_stmt *stmt = text_pc_getstmt(p->pc, p->t);
	switch (p->mode) {
	case MODE_FINDSETUP:
		return ast_stmt_pushsetup(stmt, s);
	case MODE_SETUP:
	case MODE_ABSTRACT:
	case MODE_ACTUAL:
		return ast_stmt_exec(stmt, s);
	case MODE_VERIFY:
		return ast_stmt_verify(stmt, s);
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
	case STATE_ATEND:
		return program_step(p, s);
	case STATE_STMTS:
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
	case STATE_STMTS:
		return lexememarker_str(
			ast_stmt_lexememarker(text_pc_getstmt(p->pc, p->t))
		);
	case STATE_ATEND:
		return lexememarker_str(
			ast_stmt_lexememarker(text_pc_prevstmt(p->pc, p->t))
		);
	default:
		assert(false);
	}
}

struct lexememarker *
program_lexememarker(struct program *p)
{
	switch (p->s) {
	case STATE_STMTS:
		return ast_stmt_lexememarker(text_pc_getstmt(p->pc, p->t));
	case STATE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}
