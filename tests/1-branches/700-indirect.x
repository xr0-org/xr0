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
number()
{
	return 0;
}

void *
test() [
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
