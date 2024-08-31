#ifndef COMMAND_H
#define COMMAND_H

struct verifier;

struct error *
command_next(struct verifier *, char *debugsep);

#endif
