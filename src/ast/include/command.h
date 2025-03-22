#ifndef COMMAND_H
#define COMMAND_H

#include "util.h"

struct command;

DECLARE_RESULT_TYPE(struct command *, command, command_res)

struct command *
command_copy(struct command *);

void
command_destroy(struct command *);

char *
command_str(struct command *);

struct verifier;

struct error *
command_exec(struct verifier *, char *debugsep);

#endif
