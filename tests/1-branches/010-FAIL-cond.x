#include <stdlib.h>

void
foo(void **pp) [ if (@pp) {} ]
{
	void *q;

	q = malloc(4);
	pp[0] = q;
}
