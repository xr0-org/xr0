#include <stdlib.h>

void *p;

void
unit()
{
	p = malloc(1);
	free(p);
}
