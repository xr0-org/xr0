#include <stdlib.h>

int
number(int param);

#ifdef XR0

void *
alloc_if(int num) ~ [
	if (num) {
		return .malloc(1);
	}
];

void *
test(int x) ~ [
	if (number(x)) {
		return .malloc(1);
	}
];

#endif

void *
test(int x)
{
	int n;

	n = x;

	return alloc_if(number(n));
}

int
number(int param)
{
	return param;
}

void *
alloc_if(int num)
{
	if (num) {
		return malloc(1);
	}
	return NULL;
}
