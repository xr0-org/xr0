#include <stdlib.h>

void *
alloc_if(int num) [
	if (num) {
		.alloc result;
	}
]{
	if (num) {
		return malloc(1);
	}
	return NULL;
}

int
number(int param)
{
	return param;
}

void *
test(int x) [
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
