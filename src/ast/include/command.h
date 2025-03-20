#ifndef COMMAND_H
#define COMMAND_H

struct verifier;

struct error *
command_exec(struct verifier *, char *debugsep);

#endif
