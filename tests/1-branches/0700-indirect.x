#include <stdlib.h>

int
number();

void *
alloc_if(int num) ~ [
	if (num) {
		result = .alloc(1);
	}
];

void *
test() ~ [
	if (number()) {
		result = .alloc(1);
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
