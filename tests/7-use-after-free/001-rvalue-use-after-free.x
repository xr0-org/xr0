#include <stdlib.h>

void
func()
{
	int *p;
	int q;

	p = malloc(1);
	free(p);

	q = *p;		/* ERROR: unjustified indirection (rvalue) */
}
