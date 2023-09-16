#include <stdlib.h>

void
unit() 
{
	void *p;

	p = malloc(1);
	free(p);
	free(p);
}
