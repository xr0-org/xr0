#include <stdlib.h>

void *
unit() ~ [ .alloc result; ]
{
	void *p;

	p = malloc(1);
	free(p);
	return p;
}
