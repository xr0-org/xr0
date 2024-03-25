#include <stdlib.h>

void *
unit() ~ [ result = .alloc(1); ]
{
	void *p;

	p = malloc(1);
	free(p);
	return p;
}
