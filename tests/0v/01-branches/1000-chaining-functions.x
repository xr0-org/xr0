#include <stdlib.h>

struct tuple { int x; int y; };

struct tuple
tuple_create();

void *
conditional_alloc(int x) ~ [ if (x) return .malloc(1); ];

void *
test() ~ [ if (tuple_create().x) return .malloc(1); ]
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
