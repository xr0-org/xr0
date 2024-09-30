#include <stdlib.h>

#ifdef XR0

void *
f() ~ [ return malloc(1); ];

#endif

void *
f()
{
	free(malloc(1));

	free(malloc(1));
	free(malloc(1));
	return malloc(1);
}
