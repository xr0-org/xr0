#include <stdlib.h>

void *
leak() ~ [ return .alloc(1); ]
{
	void *p; void *q;

	p = malloc(1);
	q = malloc(1);
	return q;
}
