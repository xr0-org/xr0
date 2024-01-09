#include <stdlib.h>

int
number(int param);

void *
alloc_if(int num) ~ [
	if (num) {
		.alloc result;
	}
];

void *
test(int x) ~ [
	if (number(x)) {
		.alloc result;
	}
]{
	int num;
	void *p;

	num = number(x);
	p = alloc_if(num);

	return p;
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


