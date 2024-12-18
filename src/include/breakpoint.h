#ifndef BREAKPOINT_H
#define BREAKPOINT_H

struct breakpoint;

char *
breakpoint_list(void);

struct error *
breakpoint_set(char *filename, int linenum);

struct error *
breakpoint_delete(int id);

struct lexememarker;

bool
breakpoint_shouldbreak(struct lexememarker *);

void
breakpoint_reset(void);

#endif
