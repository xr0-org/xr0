#include <stdlib.h>

void *
unit() [ .alloc result; ]
{
	void *p; void *q;

	p = malloc(1);
	q = malloc(1);
	free(q);
	return p;
}
