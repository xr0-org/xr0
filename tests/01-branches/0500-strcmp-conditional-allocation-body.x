#include <stdlib.h>
#include <string.h>

void *
test(char *s) ~ [
	if (strcmp(s, "yes")) {
		return .malloc(1);
	}
]{
	if (strcmp(s, "yes")) {
		return malloc(1);
	}
	return NULL;
}
