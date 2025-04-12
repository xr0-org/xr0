#ifndef XR0_VERIFIER_INV_VERIFIER_H
#define XR0_VERIFIER_INV_VERIFIER_H

struct inv_verifier;

struct state;

struct inv_verifier *
inv_verifier_create(struct state *);

void
inv_verifier_destroy(struct inv_verifier *);

char *
inv_verifier_str(struct inv_verifier *);

struct error;

struct error *
inv_verifier_progress(struct inv_verifier *v, progressor *prog);

int
inv_verifier_atend(struct inv_verifier *);

struct lexememarker;

struct lexememarker *
inv_verifier_lexememarker(struct inv_verifier *);

#endif
