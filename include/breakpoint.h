#ifndef BREAKPOINT_H
#define BREAKPOINT_H

struct breakpoint;

char *
breakpoint_list();

struct error *
breakpoint_set(struct breakpoint);

struct error *
breakpoint_delete(int id);

struct lexememarker;

bool
breakpoint_shouldbreak(struct lexememarker *);

#endif
