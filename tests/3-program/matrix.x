#include <stdlib.h>
#include <stdio.h>
#include "matrix.h"

struct matrix *
matrix_create(int rows, int cols)
{
	int i;
	struct matrix *m;

	m = malloc(sizeof(struct matrix));

	m->rows = rows;
	m->cols = cols;

	m->data = malloc(sizeof(double *) * rows);
	for (i = 0; i < rows; i++) {
		m->data[i] = malloc(sizeof(double) * cols);
	}

	return m;
}

void
matrix_destroy(struct matrix *m)
{
	int i;

	for (i = 0; i < m->rows; i++) {
		free(m->data[i]);
	}
	free(m->data);
	free(m);
}

struct matrix *
matrix_add(struct matrix* m1, struct matrix *m2)
{
	int i; int j;
	struct matrix *res;

	assert(m1->rows == m2->rows && m1->cols == m2->cols);

	res = matrix_create(m1->rows, m1->cols); 
	for (i = 0; i < m1->rows; i++) {
		for (j = 0; j < m1->cols; j++) {
			res->data[i][j] = m1->data[i][j] + m2->data[i][j];
		}
	}
	return res;
}

void
matrix_print(struct matrix *m)
{
	int i; int j;

	for (i = 0; i < m->rows; i++) {
		for (j = 0; j < m->cols; j++) {
			printf("%1f ", m->data[i][j]);
		}
		printf("\n");
	}
	printf("\n\n");
}
