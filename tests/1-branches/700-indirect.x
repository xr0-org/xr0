#include <stdlib.h>

int
number();

void *
alloc_if(int num) ~ [
	if (num) {
		.alloc result;
	}
];

void *
test() ~ [
	if (number()) {
		.alloc result;
	}
]{
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
