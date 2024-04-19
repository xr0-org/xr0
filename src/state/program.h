#ifndef PROGRAM_H
#define PROGRAM_H

struct program;

struct program *
program_create(struct ast_block *, char *name);

struct program *
program_copy(struct program *);

void
program_destroy(struct program *);

char *
program_name(struct program *);

char *
program_str(struct program *);

char *
program_render(struct program *);

void
program_changename(struct program *, char *);

bool
program_atend(struct program *);

struct error *
program_exec(struct program *, bool abstract, struct state *);

char *
program_loc(struct program *);

#endif
