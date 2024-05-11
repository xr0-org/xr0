#include <stdlib.h>

void
f(int cond) ~ [ if (cond) return malloc(1); ]
{
	if (cond) {
		return malloc(1);
	}
	return NULL;
}
