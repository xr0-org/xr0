#ifndef XR0_VERIFIER_INV_MUX
#define XR0_VERIFIER_INV_MUX

struct inv_verifier;
struct inv_verifier_arr;

/* inv_mux: multiplexer for splittings of inv_verifiers */
struct inv_mux;

struct inv_mux *
inv_mux_create(struct inv_verifier_arr *);

void
inv_mux_destroy(struct inv_mux *);

int
inv_mux_atend(struct inv_mux *);

struct inv_verifier *
inv_mux_active_inv_verifier(struct inv_mux *);

#endif
