#include <stdlib.h>

#ifdef XR0

void *
unit() ~ [ return .malloc(1); ];

#endif

void *
unit()
{
	void *p; void *q;

	p = malloc(1);
	q = p;
	return q;
}
