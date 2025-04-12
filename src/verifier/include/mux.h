#ifndef XR0_VERIFIER_MUX
#define XR0_VERIFIER_MUX

struct mux;

typedef int (*plex_atend)(void *);
typedef int (*plex_destroy)(void *);

struct mux *
mux_create(void *init, plex_atend, plex_destroy);

void
mux_destroy(struct mux *);

int
mux_atend(struct mux *);

void *
mux_plex(struct mux *);

void *
mux_split(void *, void *);

#endif
