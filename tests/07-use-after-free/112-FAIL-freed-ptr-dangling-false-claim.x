#include <stdlib.h>

int *
func() ~ [ return $; ]	/* ERROR: */
{
	int *p;
	p = malloc(1);

	free(p);	/* p dangling */

	return p;	/* return dangling ptr */
}

int
main()
{
	int *i;
	int *j;

	i = func();	/* should work based on func abstract? */
	j = *i;

	return 0;
}
