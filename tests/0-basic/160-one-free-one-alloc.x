#include <stdlib.h>

void *
unit() ~ [ return .malloc(1); ]
{
	void *p; void *q;

	p = malloc(1);
	q = malloc(1);
	free(q);
	return p;
}
