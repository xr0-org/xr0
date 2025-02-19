#ifndef XR0_VALUE_CCONST_H
#define XR0_VALUE_CCONST_H

/* cconst: A compile-time constant, like literal constant or one of the min/max
 * values such as INT_MIN and INT_MAX. */
struct cconst;

struct cconst *
cconst_constant_create(int);

struct cconst *
cconst_min_create(void);

struct cconst *
cconst_max_create(void);

struct cconst *
cconst_copy(struct cconst *v);

void
cconst_destroy(struct cconst *);

char *
cconst_str(struct cconst *);

char *
cconst_str_inrange(struct cconst *);

int
cconst_ismax(struct cconst *);

int
cconst_ismin(struct cconst *);

int
cconst_isconstant(struct cconst *);

int
cconst_as_constant(struct cconst *);

int
cconst_lt(struct cconst *lhs, struct cconst *rhs);

int
cconst_eq(struct cconst *lhs, struct cconst *rhs);

int
cconst_le(struct cconst *lhs, struct cconst *rhs);

int
cconst_ge(struct cconst *lhs, struct cconst *rhs);

int
cconsts_aresinglerange(struct cconst *lw, struct cconst *up);

#endif
