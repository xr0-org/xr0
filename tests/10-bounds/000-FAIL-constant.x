#include <stdlib.h>

void
foo()
{
	int i; int *p;
	p = &i;
	p[1] = 1;
}
