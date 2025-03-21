#ifndef COMMAND_H
#define COMMAND_H

struct command;

DECLARE_RESULT_TYPE(struct command *, command, command_res)

struct verifier;

struct error *
command_exec(struct verifier *, char *debugsep);

#endif
