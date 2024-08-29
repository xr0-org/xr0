#include <stdlib.h>

int
f(int x, int y) ~ [ return 0; ]
{
	return 0;
}

int
g(int x) ~ [ return 1; ]
{
	return 1;
}

int
h(int x, int y) ~ [ return 2; ]
{
	return 2;
}

void *
test(int x, int y) ~ [ return .malloc(1); ]
{
	if (f(g(x), h(f(x, y), f(g(x), y)))) {
		return malloc(1);
	}
	return malloc(1);
}
