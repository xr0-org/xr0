#include <stdlib.h>

void
unit()
{
	void *p; void *q;

	p = malloc(1);
	q = p;
	free(q);
	free(p);
}
