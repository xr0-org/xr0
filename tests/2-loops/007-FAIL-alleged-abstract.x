#include <stdlib.h>

void
unit()
{
	int i;
	void **p;

	p = malloc(9);

	for (i = 0; i < 9; i++) [ .alloc p[i]; ] {}

	/* should fail loop internal consistency check */
}
