#ifndef XR0_LSI_TALLY_H
#define XR0_LSI_TALLY_H

struct tally;

struct tally *
tally_create(void);

void
tally_destroy(struct tally *);

char *
tally_str(struct tally *);

int
tally_getcoef(struct tally *, char *var);

int
tally_getconst(struct tally *);

void
tally_setcoef(struct tally *, char *var, int);

void
tally_setconst(struct tally *, int);

struct string_arr;

struct string_arr *
tally_getvars(struct tally *);

#endif
