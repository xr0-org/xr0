#include <stdlib.h>
#include <stdio.h>

struct matrix {
	int rows; int cols;
	int **data;
};

struct matrix *
matrix_create(int rows, int cols) ~ [
	struct matrix *m;
	int i;

	m = .malloc(sizeof(struct matrix));
	m->data = .malloc(sizeof(int *) * rows);
	m->rows = rows;
	m->cols = cols;
	for (i = 0; i < m->rows; i++) {
		m->data[i] = .malloc(sizeof(int));	
	}
	return m;
]{
	int i;
	struct matrix *m;

	m = malloc(sizeof(struct matrix));

	m->rows = rows;
	m->cols = cols;

	m->data = malloc(sizeof(int *) * rows);
	for (i = 0; i < rows; i++) ~ [ m->data[i] = .malloc(sizeof(int *) * cols); ] {
		m->data[i] = malloc(sizeof(int *) * cols);
	}
	return m;
}

void
matrix_destroy(struct matrix *m) ~ [
	int i;

	setup: m = matrix_create($, $);

	for (i = 0; i < m->rows; i++) {
		.free(m->data[i]);
	}
	.free(m->data);
	.free(m);
]{
	int i;

	for (i = 0; i < m->rows; i++) ~ [ .free(m->data[i]); ] {
		free(m->data[i]);
	}
	free(m->data);
	free(m);
}

struct matrix *
matrix_add(struct matrix *m1, struct matrix *m2) ~ [
	int i;
	struct matrix *sum;

	setup: {
		m1 = matrix_create($, $);
		m2 = matrix_create($, $);
	}

	sum = .malloc(sizeof(struct matrix *));
	sum->data = .malloc(sizeof(int *) * m1->rows);
	sum->rows = m1->rows;
	sum->cols = m1->cols;
	for (i = 0; i < sum->rows; i++) {
		sum->data[i] = .malloc(sizeof(int));
	}
	return sum;
]{
	int i; int j;
	struct matrix *res;

	/*assert(m1->rows == m2->rows && m1->cols == m2->cols);*/

	res = matrix_create(m1->rows, m1->cols);
	for (i = 0; i < res->rows; i++) {
		for (j = 0; j < m1->cols; j++) {
			res->data[i][j] = m1->data[i][j] + m2->data[i][j];
		}
	}
	return res;
}

void
matrix_print(struct matrix *m) ~ [
	setup: m = matrix_create($, $);
] {
	int i; int j; int digit;

	for (i = 0; i < m->rows; i++) {
		for (j = 0; j < m->cols; j++) {
			digit = m->data[i][j] + '0';
			putchar(digit);
		}
		puts("\n");
	}
	puts("\n");
}

int
main()
{
	struct matrix *m1;
	struct matrix *m2;
	struct matrix *sum;

	puts("matrix program:\n");
	m1 = matrix_create(2, 2);
	~ [ @m1; ]

	m1->data[0][0] = 1;
	m1->data[0][1] = 2;
	m1->data[1][0] = 3;
	m1->data[1][1] = 4;
	puts("m1:\n");
	matrix_print(m1);

	m2 = matrix_create(2, 2);
	~ [ @m1; @m2; ]

	m2->data[0][0] = 1;
	m2->data[0][1] = 1;
	m2->data[1][0] = 1;
	m2->data[1][1] = 1;
	puts("m2:\n");
	matrix_print(m2);

	sum = matrix_add(m1, m2);
	~ [ @m1; @m2; @sum; ]

	puts("sum:\n");
	matrix_print(sum);

	matrix_destroy(sum);
	~ [ @m1; @m2; !@sum; ]
	matrix_destroy(m2);
	~ [ @m1; !@m2; !@sum; ]
	matrix_destroy(m1);
	~ [ !@m1; !@m2; !@sum; ]
}
