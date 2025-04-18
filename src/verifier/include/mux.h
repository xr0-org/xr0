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

typedef int (*atend_func)(struct state *);

struct mux *
mux_firstactive(struct mux *, atend_func);

int
mux_isactive(struct mux *, atend_func);

struct state *
mux_state(struct mux *, atend_func);

struct error;

struct error *
mux_progress(struct mux *, progressor *, atend_func);

struct error *
mux_one_verifies(struct mux *, struct state *);

void
mux_endinvariant(struct mux *);

#endif
