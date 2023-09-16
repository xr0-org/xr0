#include <stdlib.h>

void *p;

void
unit() [ .alloc p; ]
{
	p = malloc(1);
}
