#include <stdlib.h>

void
foo()
{
	int **p;
	int res;
	int temp;
	p = malloc(2);
	p[1] = malloc(3);

	*p[1] = 1;
	*(p[1]+1) = 2;
	*(p[1]+2) = 3;
	temp = *p[1];
	~ [ temp == 1; ]
	temp = *(p[1]+1);
	~ [ temp == 2; ]
	temp = *(p[1]+2);
	~ [ temp == 3; ]	/* XXX: broken */

	*(p[1]+3) = 3;		/* ERROR: out of bounds */
}
