#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "math.h"
#include "util.h"

static struct math_expr *
integer(int);

static struct math_expr *
variable(char *);

int
main()
{
	/* ((x + 1) - y) + (y + x) */
	struct math_expr *e1 = math_expr_sum_create(
		math_expr_sum_create(
			math_expr_sum_create(
				variable("x"),
				integer(1)
			),
			math_expr_neg_create(variable("y"))
		),
		math_expr_sum_create(
			variable("y"),
			variable("x")
		)
	);

	/* (y + (x - 2)) + (x + (3 - y)) */
	struct math_expr *e2 = math_expr_sum_create(
		math_expr_sum_create(
			variable("y"),
			math_expr_sum_create(
				variable("x"),
				integer(-2)
			)
		),
		math_expr_sum_create(
			variable("x"),
			math_expr_sum_create(
				integer(3),
				math_expr_neg_create(variable("y"))
			)
		)
	);


	char *s1 = math_expr_str(e1),
	     *s2 = math_expr_str(e2);
	printf("e1: %s, e2: %s\n", s1, s2);
	free(s2); free(s1);

	assert(math_le(e1, e2));
	assert(math_le(e2, e1));

	math_expr_destroy(e2);

	math_expr_destroy(e1);
}

static struct math_expr *
integer(int i)
{
	if (i < 0) {
		return math_expr_neg_create(integer(-i));
	}
	return math_expr_atom_create(math_atom_nat_create(i));
}

static struct math_expr *
variable(char *x)
{
	return math_expr_atom_create(math_atom_variable_create(dynamic_str(x)));
}
