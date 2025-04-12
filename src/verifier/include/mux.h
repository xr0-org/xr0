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

typedef int (*verifier_rule)(struct verifier *);

int
mux_all(struct mux *, verifier_rule);

struct verifier *
mux_firstnot(struct mux *, verifier_rule);

struct error;
struct state;

struct error *
mux_one_verifies(struct mux *, struct state *);

#endif
