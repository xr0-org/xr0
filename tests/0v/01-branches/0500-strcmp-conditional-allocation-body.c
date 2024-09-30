#include <stdlib.h>
#include <string.h>

#ifdef XR0

void *
test(char *s) ~ [
	if (strcmp(s, "yes")) {
		return .malloc(1);
	}
];

#endif

void *
test(char *s)
{
	if (strcmp(s, "yes")) {
		return malloc(1);
	}
	return NULL;
}
