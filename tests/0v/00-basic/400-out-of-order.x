#include <stdlib.h>

void *
f() ~ [ return malloc(1); ]
{
	free(malloc(1));

	free(malloc(1));
	free(malloc(1));
	return malloc(1);
}
