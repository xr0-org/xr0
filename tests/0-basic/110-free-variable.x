#include <stdlib.h>

void
unit()
{
	void *p;

	p = malloc(1);
	[ @p; ]
	free(p);
	[ !@p; ]
}
