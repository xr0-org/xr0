#include <stdlib.h>

int *
func()
{
	int *p;
	p = malloc(1);

	free(p);	/* p dangling */

	#ifdef XR0
	~ [ !@p; ];
	#endif

	return p;	/* return dangling ptr */
}

int
main()
{
	int *i;
	int *j;

	i = func();
	j = *i;		/* ERROR: rconst not lvalue */

	return 0;
}
