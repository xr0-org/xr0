#include <stdlib.h>

void
unit()
{
	int i;
	int j;
	void **p;

	p = malloc(9);

	for (i = 0; i != 9; i++) [
		for (j = 0; j != i; j++) { .alloc p[j]; }
		for (j = 0; j != 9; j++) { .alloc p[j]; }
	] {
		p[i] = malloc(1);
	}

	for (i = 0; i != 9; i++) [
		for (j = 0; j != i; j++) { .dealloc p[j]; }
		for (j = 0; j != 9; j++) { .dealloc p[j]; }
	] {
		free(p[i]);
	}

	free(p);
}
