#include <stdlib.h>

int *
func()
{
	int *p;
	p = malloc(1);

	free(p);	/* p dangling */
	~ [ !@p; ];
	return p;	/* return dangling ptr */
}

int
main()
{
	int *i;
	int *j;

	i = func();
	j = *i;		/* ERROR: undefined indirection (rvalue) */

	return 0;
}
