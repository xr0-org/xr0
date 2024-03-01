#include <stdlib.h>

int *
func() ~ [ $result; /* false claim */ ];

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
func()
{
	int *p;
	p = malloc(1);
	free(p);	/* p dangling */

	return p;	/* return dangling ptr */
}
