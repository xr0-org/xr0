#include <stdlib.h>

void
unit()
{
	void **p;
	void **q;

	p = malloc(1); /* `infinite' block */

	p[5] = malloc(1);

	q = malloc(1);

	q[3] = p[5];

	free(p);

	free(q[3]);

	free(q);
}
