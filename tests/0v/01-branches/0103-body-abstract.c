#include <stdlib.h>

#ifdef XR0

void *
f(int cond) ~ [ if (cond) return malloc(1); ];

#endif

void *
f(int cond)
{
	if (cond) {
		return malloc(1);
	}
	return NULL;
}
