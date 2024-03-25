#include <stdlib.h>
#include <string.h>

void *
test(char *s) ~ [
	if (strcmp(s, "yes") == 0) {
		return .alloc(1);
	}
	if (!(strcmp(s, "yes") == 0)) {
		return .alloc(1);
	}
]{
	return malloc(1);
}
