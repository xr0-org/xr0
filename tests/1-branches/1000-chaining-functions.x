#include <stdlib.h>

struct tuple { int x; int y; };

struct tuple
tuple_create() ~ [
	result.x = $;
	result.y = $;
];

int
condition(int x);

void *
conditional_alloc(int x) ~ [
	if (condition(x)) {
		.alloc result;
	}
];

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

int
condition(int x)
{
	return 0;
}

void *
conditional_alloc(int x)
{
	if (condition(x)) {
		return malloc(1);
	}
	return NULL;
}
