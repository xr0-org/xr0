#include <stdlib.h>
void *p;

void
unit() [ if (@p) { .dealloc p; } else { .undefined; } ]
{
	free(p);
}
