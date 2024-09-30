#include <stdlib.h>

void
f(int *p) ~ [ setup: p = malloc(1); ]
{
	*p = 5;
}
