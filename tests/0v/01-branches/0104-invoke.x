#include <stdlib.h>

void *
f(int cond) ~ [ if (cond) return malloc(1); ]
{
	if (!cond) {
		return NULL;
	}
	return malloc(1);
}

void
g()
{
	void *p = f(1);
	free(p);
}
