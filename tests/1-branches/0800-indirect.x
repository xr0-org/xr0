#include <stdlib.h>

int
number(int param);

void *
alloc_if(int num) ~ [
	if (num) {
		result = .alloc(1);
	}
];

void *
test(int x) ~ [
	if (number(x)) {
		result = .alloc(1);
	}
]{
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


