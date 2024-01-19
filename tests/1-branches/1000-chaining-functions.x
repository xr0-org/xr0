#include <stdlib.h>

struct tuple {
	int x;
	void *p;
};

int
condition(int x);

struct tuple
tuple_create(int x) ~ [
	result.p = $;
	if (condition(x)) {
		.alloc result.p;
	}
	result.x = $;
]{
	struct tuple t;

	t.x = x;
	t.p = NULL;
	if (condition(x)) {
		t.p = malloc(1);
	}
	return t;
}

int
condition(int x)
{
	return 0;
}

void
test(int x)
{
	struct tuple t;

	t = tuple_create(x);
	if (condition(x)) {
		free(t.p);
	}
	t = tuple_create(t.x);
	if (condition(t.x)) {
		free(t.p);
	}
}
