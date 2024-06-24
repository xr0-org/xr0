#include <stdlib.h>

void
foo()
{
	int **p;
	int res;
	p = malloc(2);
	p[1] = malloc(3);

	*p[1] = 1;
	*(p[1]+1) = 2;
	*(p[1]+2) = 3;
	~ [ *p[1] == 1; ]
	~ [ *(p[1]+1) == 2; ]
	~ [ *(p[1]+2) == 2; ]	/* XXX: broken */

	*(p[1]+3) = 3;		/* ERROR: out of bounds */
}
