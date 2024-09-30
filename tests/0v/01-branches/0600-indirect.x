#include <stdlib.h>

#ifdef XR0

void *
alloc_if(int num) ~ [
	if (num) {
		return .malloc(1);
	}
];

void *
test(int x) ~ [
	if (x) {
		return .malloc(1);
	}
];

#endif

void *
alloc_if(int num)
{
	if (num) {
		return malloc(1);
	}
	return NULL;
}

void *
test(int x)
{
	void *p;

	p = alloc_if(x);

	return p;
}
