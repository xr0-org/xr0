#include <stdlib.h>

void *
unit() [ .alloc result; ]
{
	void *p;

	p = malloc(1);
	[ @p; ]
	return p;
}
