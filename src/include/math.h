#ifndef XR0_MATH_KERNEL_H
#define XR0_MATH_KERNEL_H

#include <stdbool.h>

struct math_expr;

/* math_le: e1 ≤ e2 */
bool
math_le(struct math_expr *e1, struct math_expr *e2);

bool
math_eq(struct math_expr *e1, struct math_expr *e2);

bool
math_lt(struct math_expr *e1, struct math_expr *e2);

bool
math_gt(struct math_expr *e1, struct math_expr *e2);

bool
math_ge(struct math_expr *e1, struct math_expr *e2);


struct math_atom;

struct math_expr *
math_expr_atom_create(struct math_atom *);

struct math_expr *
math_expr_sum_create(struct math_expr *, struct math_expr *); 

struct math_expr *
math_expr_neg_create(struct math_expr *);

struct math_expr *
math_expr_copy(struct math_expr *);

void
math_expr_destroy(struct math_expr *);

char *
math_expr_str(struct math_expr *);

struct math_expr *
math_expr_simplify(struct math_expr *raw);


struct math_atom *
math_atom_nat_create(unsigned int);

struct math_atom *
math_atom_variable_create(char *);

struct math_atom *
math_atom_copy(struct math_atom *);

void
math_atom_destroy(struct math_atom *);

char *
math_atom_str(struct math_atom *);

#endif
