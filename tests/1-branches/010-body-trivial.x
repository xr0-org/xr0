#include <stdlib.h>
#include <string.h>

void *
test(int x) [ .alloc result; ]
{
	if (x) {
		return malloc(1);
	}
	return malloc(1);
}
