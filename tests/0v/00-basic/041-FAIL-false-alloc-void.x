#include <stdlib.h>

#ifdef XR0

void *
unit() ~ [ return .malloc(1); ];

#endif

void *
unit() ~ [ return .malloc(1); ]
{
	return NULL;
}
