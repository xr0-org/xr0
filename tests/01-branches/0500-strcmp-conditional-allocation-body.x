#include <stdlib.h>
#include <string.h>

void *
test(char *s) ~ [
	if (strcmp(s, "yes") == 0) {
		return .malloc(1);
	}
]{
	if (strcmp(s, "yes") == 0) {
		return malloc(1);
	}
	return NULL;
}
