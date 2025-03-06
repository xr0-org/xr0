#ifndef XR0_LSI_TALLY_H
#define XR0_LSI_TALLY_H

struct tally;

struct tally *
tally_create(void);

struct tally *
tally_copy(struct tally *);

void
tally_destroy(struct tally *);

char *
tally_str(struct tally *);

long
tally_getval(struct tally *, char *var);

long
tally_getconst(struct tally *);

void
tally_setval(struct tally *, char *var, long);

void
tally_setconst(struct tally *, long);

struct tally *
tally_sum(struct tally *, struct tally *);

struct tally *
tally_product(struct tally *, long);

struct string_arr;

struct string_arr *
tally_getvars(struct tally *);

#endif
