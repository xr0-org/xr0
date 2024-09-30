#include <stdlib.h>

#ifdef XR0

void
foo(int cond, void *p) ~ [
	if (cond) {
		setup: p = malloc(1);
	}
];

#endif

void
foo(int cond, void *p)
{
	/* empty definition */
}

test(int x)
{
	void *p;

	p = malloc(1);
	foo(x, p);
	free(p);
}
