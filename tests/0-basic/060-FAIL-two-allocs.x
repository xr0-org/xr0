#include <stdlib.h>

void *
leak() [ .alloc result; ]
{
	void *p; void *q;

	p = malloc(1);
	q = malloc(1);
	return q;
}
