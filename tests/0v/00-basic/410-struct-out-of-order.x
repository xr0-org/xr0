#include <stdlib.h>

struct pair {
	void *p;
	void *q;
};

#ifdef XR0

struct pair *
f() ~ [
	struct pair *pair;

	pair = malloc(sizeof(struct pair));
	pair->p = malloc(1);
	pair->q = malloc(1);
	return pair;
];

#endif

struct pair *
f()
{
	struct pair *pair;
	void *q;

	q = malloc(1);
	pair = malloc(sizeof(struct pair));
	pair->p = malloc(1);
	pair->q = q;
	return pair;
}
