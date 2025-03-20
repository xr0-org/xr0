#ifndef COMMAND_H
#define COMMAND_H

struct verifier;

struct command;

struct command *
command_read(char *debugsep);

struct error *
command_exec(struct verifier *, struct command *, char *debugsep);

#endif
