#include <stdio.h>
#include "matrix.h"

int
main()
{
	printf("matrix program:\n");

	struct matrix *m1 = matrix_create(2, 2);
	[ allocated(m1); ]

	m1->data[0][0] = 1;
	m1->data[0][1] = 2;
	m1->data[1][0] = 3;
	m1->data[1][1] = 4;
	printf("m1:\n");
	matrix_print(m1);

	struct matrix *m2 = matrix_create(2, 2);
	[ allocated(m1); allocated(m2); ]
	

	m2->data[0][0] = 1;
	m2->data[0][1] = 1;
	m2->data[1][0] = 1;
	m2->data[1][1] = 1;
	printf("m2:\n");
	matrix_print(m2);

	struct matrix *sum = matrix_add(m1, m2);
	[ allocated(m1); allocated(m2); allocated(sum); ]

	printf("sum:\n");
	matrix_print(sum);

	matrix_destroy(sum);
	matrix_destroy(m2);
	matrix_destroy(m1);
}
