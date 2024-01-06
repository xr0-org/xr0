#ifndef XR0_STATE_INTERN_H
#define XR0_STATE_INTERN_H

struct location;
struct object;

struct object *
state_get(struct state *state, struct location *loc, bool constructive);

struct block *
state_getblock(struct state *state, struct location *loc);

bool
state_references(struct state *s, struct location *loc);

#endif
