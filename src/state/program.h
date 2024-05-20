#ifndef PROGRAM_H
#define PROGRAM_H

struct program;

struct program *
program_create(struct ast_block *);

struct program *
program_copy(struct program *);

void
program_destroy(struct program *);

char *
program_str(struct program *);

int
program_index(struct program *);

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

enum execution_mode;

struct error *
program_step(struct program *, struct state *);

struct error *
program_next(struct program *, struct state *);

char *
program_loc(struct program *);

struct lexememarker *
program_lexememarker(struct program *);

#endif
