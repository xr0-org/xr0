#include <stdlib.h>

int *
func() ~ [ !$result; ]
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

	i = func();
	j = *i;		/* ERROR: unjusitified indirection */

	return 0;
}
