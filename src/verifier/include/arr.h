#ifndef XR0_VERIFIER_ARR
#define XR0_VERIFIER_ARR

struct verifier;

struct verifier_arr;

struct verifier_arr *
verifier_arr_create(void);

void
verifier_arr_destroy(struct verifier_arr *);

int
verifier_arr_n(struct verifier_arr *);

struct verifier **
verifier_arr_paths(struct verifier_arr *);

int
verifier_arr_append(struct verifier_arr *, struct verifier *);

#endif
