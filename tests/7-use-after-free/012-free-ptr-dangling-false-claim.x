#include <stdlib.h>

int
main()
{
	int *i;
	int *j;

	i = func();
	j = *i;		/* ERROR: unjusitified indirection */

	return 0;
}

int *
func() ~ [
	$result;	/* false claim */
] {
	int *p;
	p = malloc(1);
	free(p);	/* p dangling */

	return p;	/* return dangling ptr */
}
