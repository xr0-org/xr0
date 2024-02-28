#include <stdlib.h>

void
func()
{
	int *p;

	p = malloc(1);
	free(p);

	*p = 1;		/* ERROR: undefined indirection (lvalue) */
}
