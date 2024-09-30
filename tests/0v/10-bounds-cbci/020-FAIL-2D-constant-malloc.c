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

	#ifdef XR0
	~ [ temp == 1; ]
	#endif

	temp = *(p[1]+1);

	#ifdef XR0
	~ [ temp == 2; ]
	#endif 

	temp = *(p[1]+2);

	#ifdef XR0
	~ [ temp == 3; ]	/* XXX: broken */
	#endif

	*(p[1]+3) = 3;		/* ERROR: out of bounds */
}
