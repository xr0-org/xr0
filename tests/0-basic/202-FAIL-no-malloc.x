#include <stdlib.h>

void *p;

void
unit() [ .alloc p; ]
{
	p = NULL;
}
