#include <stdlib.h>

void *
unit(int K) [ if (K < 3) .alloc result; ]
{
	void *p;
	if (K < 3) {
		p = malloc(1);
		return p;
	}
	p = NULL;
	return p;
}
