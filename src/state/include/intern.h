#ifndef XR0_STATE_INTERN_H
#define XR0_STATE_INTERN_H

struct location;
struct object;

struct object_res *
state_get(struct state *, struct rconst *, struct location *, bool constructive);

struct block *
state_getblock(struct state *, struct rconst *, struct location *);

bool
state_references(struct state *, struct location *);

#endif
