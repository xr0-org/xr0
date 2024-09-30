#include <stdlib.h>

int
number();


#ifdef XR0

void *
alloc_if(int num) ~ [
	if (num) {
		return .malloc(1);
	}
];

void *
test() ~ [
	if (number()) {
		return .malloc(1);
	}
];

#endif

void *
test()
{
	int num;
	void *p;

	num = number();
	p = alloc_if(num);

	return p;
}

int
number()
{
	return 0;
}

void *
alloc_if(int num)
{
	if (num) {
		return malloc(1);
	}
	return NULL;
}
