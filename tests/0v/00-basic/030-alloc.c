#include <stdlib.h>

#ifdef XR0

void *
unit() ~ [ return .malloc(1); ];

#endif

void *
unit()
{
	void *p;

	p = malloc(1);
	return p;
}
