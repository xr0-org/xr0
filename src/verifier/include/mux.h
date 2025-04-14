#ifndef XR0_VERIFIER_MUX
#define XR0_VERIFIER_MUX

struct state;

struct mux;

typedef int (*atend_checker)(struct state *);

struct mux *
mux_create(struct state *, atend_checker);

void
mux_destroy(struct mux *);

int
mux_isactive(struct mux *);

struct state *
mux_state(struct mux *);

struct lsi_le;

void
mux_split(struct mux *, struct lsi_le *, struct lsi_le *);

void
mux_set_atend(struct mux *, atend_checker);

struct error;

struct error *
mux_one_verifies(struct mux *, struct state *);

#endif
