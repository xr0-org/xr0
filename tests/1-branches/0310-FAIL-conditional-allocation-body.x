#include <stdlib.h>

void *
test(int x) ~ [
	if (x) {
		result = .alloc(1);
	}
]{
	return NULL;
}
