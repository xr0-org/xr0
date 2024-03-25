#include <stdlib.h>

void *
leak() ~ [ return .malloc(1); ]
{
	void *p; void *q;

	p = malloc(1);
	q = malloc(1);
	return q;
}
