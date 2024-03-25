#include <stdlib.h>

void *
alloc_if(int num) ~ [
	if (num) {
		return .malloc(1);
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
		return .malloc(1);
	}
]{
	void *p;

	p = alloc_if(x);

	return p;
}
