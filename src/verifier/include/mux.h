#ifndef XR0_VERIFIER_MUX
#define XR0_VERIFIER_MUX

struct verifier;
struct verifier_arr;

/* mux: multiplexer for splittings of verifiers */
struct mux;

struct mux *
mux_create(struct verifier_arr *);

void
mux_destroy(struct mux *);

int
mux_atend(struct mux *);

struct verifier *
mux_activeverifier(struct mux *);

void
mux_next(struct mux *);

#endif
