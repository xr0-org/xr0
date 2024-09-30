#include <stdlib.h>

#ifdef XR0

void
unit(void *p) ~ [
	setup: p = .malloc(1);

	.free(p);
];

#endif

void
unit(void *p)
{
	free(p);
}
