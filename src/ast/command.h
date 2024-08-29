#ifndef COMMAND_H
#define COMMAND_H

struct path;

struct error *
command_next(struct path *, char *debugsep);

#endif
