#include <stdlib.h>
#include <string.h>

void *
test(char *s) [ .alloc result; ]
{
	if (strcmp(s, "yes") == 0) {
		return malloc(1);
	}
	return malloc(1);
}
