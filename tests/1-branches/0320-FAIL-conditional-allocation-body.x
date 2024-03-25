#include <stdlib.h>

void *
test(int x) ~ [
	if (x) {
		return .alloc(1);
	}
]{
	return malloc(1);
}
