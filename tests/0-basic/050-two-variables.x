#include <stdlib.h>

void *
unit() ~ [ return .malloc(1); ]
{
	void *p; void *q;

	p = malloc(1);
	q = p;
	return q;
}
