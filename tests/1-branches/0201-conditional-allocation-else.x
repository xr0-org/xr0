#include <stdlib.h>

void *
test(int x) ~ [
	if (x) {
		return .malloc(1);
	} else {
		return .malloc(1);
	}
]{
	return malloc(1);
}
