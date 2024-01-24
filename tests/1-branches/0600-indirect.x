#include <stdlib.h>

void *
alloc_if(int num) ~ [
	if (num) {
		.alloc result;
	}
]{
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
]{
	void *p;

	p = alloc_if(x);

	return p;
}
