#ifndef XR0_HISTORY_H
#define XR0_HISTORY_H

struct history;

struct history *
history_create();

void
history_destroy(struct history *);

void
history_record(struct history *, int linenumber, struct state *);

struct state_arr *
history_getstates(struct history *, int linuenumber, int col);

char *
history_tojson(struct history *);

#endif
