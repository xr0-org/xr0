#include <stdlib.h>

void *
test(int x) ~ [
	if (x) {
		result = .alloc(1);
	}
]{
	if (x) {
		return malloc(1);
	}
	return NULL;
}
