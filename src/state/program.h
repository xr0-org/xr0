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
program_nextstmt(struct program *, struct state *);

char *
program_render(struct program *);

void
program_setatend(struct program *);

bool
program_atend(struct program *);

enum execution_mode;

struct error *
program_exec(struct program *, enum execution_mode, struct state *);

char *
program_loc(struct program *);

#endif
