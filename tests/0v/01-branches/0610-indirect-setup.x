#include <stdlib.h>

foo(int cond, void *p) ~ [
	if (cond) {
		setup: p = malloc(1);
	}
]{}

test(int x)
{
	void *p;

	p = malloc(1);
	foo(x, p);
	free(p);
}
