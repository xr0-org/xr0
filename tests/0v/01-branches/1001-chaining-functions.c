#include <stdlib.h>

struct tuple { int x; int y; };

#ifdef XR0

struct tuple
tuple_create() ~ [
	struct tuple t;
	t.x = [?];
	t.y = [?];
	return t;
];

void *
conditional_alloc(int x) ~ [ if (x) { return .malloc(1); } ];

void *
test() ~ [ if (tuple_create().x) { return .malloc(1); } ];

#endif

void *
test()
{
	struct tuple t;

	t = tuple_create();

	return conditional_alloc(t.x);
}

struct tuple
tuple_create()
{
	struct tuple t;

	t.x = 0;
	t.y = 0;
	return t;
}

void *
conditional_alloc(int x)
{
	if (x) {
		return malloc(1);
	}
	return NULL;
}
