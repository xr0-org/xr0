#include <stdlib.h>

void *
alloc_if(int num) ~ [
	if (num) {
		.alloc result;
	}
];

void *
alloc_if(int num)
{
	if (num) {
		return malloc(1);
	}
	return NULL;
}

void *
test(int x) ~ [
	if (x) {
		.alloc result;
	}
];

void *
test(int x)
{
	void *p;

	p = alloc_if(x);

	return p;
}
