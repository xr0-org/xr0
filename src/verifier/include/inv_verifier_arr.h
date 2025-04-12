#ifndef XR0_VERIFIER_INV_VERIFIER_ARR_H
#define XR0_VERIFIER_INV_VERIFIER_ARR_H


struct inv_verifier;

struct inv_verifier_arr;

struct inv_verifier_arr *
inv_verifier_arr_create(void);

void
inv_verifier_arr_destroy(struct inv_verifier_arr *);

int
inv_verifier_arr_n(struct inv_verifier_arr *);

struct inv_verifier **
inv_verifier_arr_iv(struct inv_verifier_arr *);

int
inv_verifier_arr_append(struct inv_verifier_arr *, struct inv_verifier *);

#endif
