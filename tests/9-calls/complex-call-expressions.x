#include <stdlib.h>

int
f(int x, int y)
{
	return 0;
}

int
g(int x)
{
	return 0;
}

int
h(int x, int y)
{
	return 0;
}

void *
test(int x, int y) ~ [ return .malloc(1); ]
{
	if (f(g(x), h(f(x, y), f(g(x), y)))) {
		return malloc(1);
	}
	return malloc(1);
}
