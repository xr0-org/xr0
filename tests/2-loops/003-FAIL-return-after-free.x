#include <stdlib.h>

void *
unit() [ .alloc result; ]
{
	int i;
	int j;
	void **p;
	void *q;

	p = malloc(5);

	for (i = 0; i != 5; i++) [
		for (j = 0; j != i; j++) { .alloc p[j]; }
		for (j = 0; j != 5; j++) { .alloc p[j]; }
	]{
		p[i] = malloc(1);
	}

	free(p[3]);

	for (i = 0; i != 3; i++) [
		for (j = 0; j != i; j++) { .dealloc p[j]; }
		for (j = 0; j != 3; j++) { .dealloc p[j]; }
	]{
		free(p[i]);	
	}

	q = p[4];

	[ @p[4]; ]

	free(p);

	return q;
}
