#include <stdlib.h>

void *
unit() ~ [ result = .alloc(1); ]
{
	void *p; void *q;

	p = malloc(1);
	q = p;
	return q;
}
