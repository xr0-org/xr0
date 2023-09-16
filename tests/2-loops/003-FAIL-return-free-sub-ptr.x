#include <stdlib.h>

void *
unit() [ .alloc result; ]
{
	int i;
	void **p;
	void *q;

	p = malloc(5);

	for (i = 0; i < 5; i++) [ .alloc p[i]; ] {
		p[i] = malloc(1);
	}

	free(p[3]);

	for (i = 0; i < 3; i++) [
		if (@p[i]) { .dealloc p[i]; } else { .undefined; }
	] {
		free(p[i]);	
	}

	q = p[4];

	[ @p[4]; ]

	free(p);

	return p[4];
}
