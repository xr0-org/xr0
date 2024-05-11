#include <stdlib.h>

struct pair {
	void *p;
	void *q;
};

struct pair *
f() ~ [
	struct pair *pair;

	pair = malloc(sizeof(struct pair));
	pair->p = malloc(1);
	pair->q = malloc(1);
	return pair;
]{
	struct pair *pair;
	void *q;

	q = malloc(1);
	pair = malloc(sizeof(struct pair));
	pair->p = malloc(1);
	pair->q = q;
	return pair;
}
