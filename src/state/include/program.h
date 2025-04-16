#ifndef PROGRAM_H
#define PROGRAM_H

struct program;

struct ast_block;

struct program *
program_abstract_create(struct ast_block *);

struct program *
program_actual_create(struct ast_block *);

struct program *
program_setup_create(struct ast_block *);

struct program *
program_findsetup_create(struct ast_block *);

struct program *
program_verify_create(struct ast_block *);

struct program *
program_nestedblock_create(struct program *origin, struct ast_block *b);

struct program *
program_linear_create(struct program *origin, struct ast_block *gen);

struct program *
program_copy(struct program *);

void
program_destroy(struct program *);

int
program_modecanverify(struct program *);

int
program_modecanrunxr0cmd(struct program *);

void
program_storeloc(struct program *);

char *
program_render(struct program *);

void
program_setatend(struct program *);

bool
program_atend(struct program *);

struct ast_expr *
program_prevcall(struct program *);

struct error *
program_step(struct program *, struct state *);

struct error *
program_next(struct program *, struct state *);

char *
program_loc(struct program *);

struct lexememarker *
program_lexememarker(struct program *);

#endif
