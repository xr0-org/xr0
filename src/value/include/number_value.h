#ifndef XR0_VALUE_NUMBER_VALUE_H
#define XR0_VALUE_NUMBER_VALUE_H

struct number_value;

struct number_value *
number_value_constant_create(int contant);

struct number_value *
number_value_min_create(void);

struct number_value *
number_value_max_create(void);

void
number_value_destroy(struct number_value *);

int
number_value_lt(struct number_value *lhs, struct number_value *rhs);

int
number_value_eq(struct number_value *lhs, struct number_value *rhs);

int
number_value_le(struct number_value *lhs, struct number_value *rhs);

int
number_value_ge(struct number_value *lhs, struct number_value *rhs);

/* XXX: either this or number_value_eq should be eliminated */
bool
number_value_equal(struct number_value *, struct number_value *);

#endif
