#include <stdlib.h>

#ifdef XR0

int
f(int x, int y) ~ [ return 0; ];

#endif

int
f(int x, int y)
{
	return 0;
}

#ifdef XR0

int
g(int x) ~ [ return 1; ];

#endif

int
g(int x)
{
	return 1;
}

#ifdef XR0

int
h(int x, int y) ~ [ return 2; ];

#endif

int
h(int x, int y)
{
	return 2;
}

#ifdef XR0

void *
test(int x, int y) ~ [ return .malloc(1); ];

#endif

void *
test(int x, int y)
{
	if (f(g(x), h(f(x, y), f(g(x), y)))) {
		return malloc(1);
	}
	return malloc(1);
}
