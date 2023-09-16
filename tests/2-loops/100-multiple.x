#include <stdlib.h>

void *
unit() [ :alloc result; ]
{
	int i;
	void **p;
	void *q;

	p = malloc(sizeof(void *) * 2);

	for (i = 0; i < 2; i++) [ .alloc p[i]; ] {
		p[i] = malloc(sizeof(void *));
	}

	free(p[0]);

	q = p[1];

	free(p);

	return q;
}
