#include <stdlib.h>

void
foo()
{
	int *p;
	p = malloc(5);
	p[0] = 5;
	p[1] = 4;
	p[2] = 3;
	p[3] = 2;
	p[4] = 1;
	p[5] = 0;
}
