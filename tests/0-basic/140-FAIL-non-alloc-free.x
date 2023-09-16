#include <stdlib.h>

void
unit() 
{
	void *p;

	p = 1;
	free(p);
}
