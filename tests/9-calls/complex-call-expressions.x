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

void *
test(int x, int y) ~ [ return .malloc(1); ]
{
	int z;
	z = g(1);
	return malloc(1);
}
