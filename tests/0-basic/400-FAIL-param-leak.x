#include <stdlib.h>

void
leak(void *p) ~ [ p = malloc(1); ]
{
	p = malloc(1);
}
