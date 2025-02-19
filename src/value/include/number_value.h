#ifndef XR0_VALUE_NUMBER_VALUE_H
#define XR0_VALUE_NUMBER_VALUE_H

struct number_value;

struct number_value *
number_value_constant_create(int contant);

struct number_value *
number_value_min_create(void);

struct number_value *
number_value_max_create(void);

struct number_value *
number_value_copy(struct number_value *v);

void
number_value_destroy(struct number_value *);

char *
number_value_str(struct number_value *);

char *
number_value_str_inrange(struct number_value *);

int
number_value_lt(struct number_value *lhs, struct number_value *rhs);

int
number_value_eq(struct number_value *lhs, struct number_value *rhs);

int
number_value_le(struct number_value *lhs, struct number_value *rhs);

int
number_value_ge(struct number_value *lhs, struct number_value *rhs);

#endif
