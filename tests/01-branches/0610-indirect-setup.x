#include <stdlib.h>

foo(int cond, int x) ~ [
	if (cond) {
		setup: x = 3;
	}
]{}

test(int x)
{
	foo(x, 3);
}
