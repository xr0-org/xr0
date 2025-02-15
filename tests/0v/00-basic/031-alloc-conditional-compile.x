#include <stdlib.h>

#ifdef XR0

void *
alloc() ~ [ return malloc(1); ];

#endif

void *
alloc()
{
	void *p;

	p = malloc(1);
	return p;
}
