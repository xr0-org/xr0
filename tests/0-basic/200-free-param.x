#include <stdlib.h>

void
unit(void *p) [ .dealloc p; ]
{
	free(p);
}
