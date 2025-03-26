#include <stdlib.h>

void *
unit() ~ [ return .malloc(1); ]
{
	void *p;

	p = malloc(1);
	return p;
}

void *
unit2() ~ [ return .malloc(1); ]
{
	void *p;

	p = malloc(1);
	p[0] = 0;
	return p;
}
