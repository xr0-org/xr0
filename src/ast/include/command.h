#ifndef COMMAND_H
#define COMMAND_H

struct verifier;

/* command */

struct command;

struct command *
command_read(char *debugsep);

struct error *
command_exec(struct verifier *, struct command *, char *debugsep);

struct command *
command_copy(struct command *);

void
command_destroy(struct command *);

char *
command_str(struct command *);

#endif
