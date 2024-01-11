#include <stdlib.h>

void *
test(int x) ~ [
	if (x) {
		.alloc result;
	}
]{
	return malloc(1);
}
