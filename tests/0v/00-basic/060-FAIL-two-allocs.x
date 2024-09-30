#include <stdlib.h>

#ifdef XR0

void *
leak() ~ [ return .malloc(1); ];

#endif

void *
leak()
{
	void *p; void *q;

	p = malloc(1);
	q = malloc(1);
	return q;
}
