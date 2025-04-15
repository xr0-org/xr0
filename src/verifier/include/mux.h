#ifndef XR0_VERIFIER_MUX
#define XR0_VERIFIER_MUX

struct state;

struct mux;

struct mux *
mux_create(struct state *);

struct mux *
mux_copy(struct mux *);

void
mux_destroy(struct mux *);

struct mux *
mux_activeleaf(struct mux *);

int
mux_isactive(struct mux *);

struct state *
mux_state(struct mux *);

struct lsi_le;

void
mux_split(struct mux *, struct lsi_le *, struct lsi_le *);

struct error;

struct error *
mux_one_verifies(struct mux *, struct state *);

void
mux_endinvariant(struct mux *);

#endif
