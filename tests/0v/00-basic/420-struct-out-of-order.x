#include <stdlib.h>

struct pair {
	void *p;
	void *q;
};

struct pair
f() ~ [
	struct pair pair;

	pair.p = malloc(1);
	pair.q = malloc(1);
	return pair;
]{
	struct pair pair;

	pair.q = malloc(1);
	pair.p = malloc(1);
	return pair;
}
