#include <stdlib.h>

void *
unit() ~ [ .alloc result; ]
{
	void *p; void *q;

	p = malloc(1);
	q = p;
	return q;
}
