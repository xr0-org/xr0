struct matrix {
	int rows, cols;
	double **data;
};

struct matrix *
matrix_create(int rows, int cols) [
	int i;

	:alloc result;
	:alloc result->data;
	for (i = 0; i < result->rows; i++) {
		:alloc result->data[i];	
	}
	result->rows == rows;
];

void
matrix_destroy(struct matrix *) [
	int i;

	for (i = 0; i < m->rows; i++) {
		:free(m->data[i]);
	}
	:free m->data;
	:free m;
];

struct matrix *
matrix_add(struct matrix *m1, struct matrix *m2) [ :alloc result; ];

void
matrix_print(struct matrix *);
