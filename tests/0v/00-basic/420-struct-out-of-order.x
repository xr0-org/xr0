#include <stdlib.h>

struct pair {
	void *p;
	void *q;
};

#ifdef XR0

struct pair
f() ~ [
	struct pair pair;

	pair.p = malloc(1);
	pair.q = malloc(1);
	return pair;
];

#endif

struct pair
f()
{
	struct pair pair;

	pair.q = malloc(1);
	pair.p = malloc(1);
	return pair;
}
