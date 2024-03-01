#include <stdlib.h>

int *
func();

int
main()
{
	int *i;
	int *j;

	i = func();
	j = *i;		/* ERROR: unjustified indirection */

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
