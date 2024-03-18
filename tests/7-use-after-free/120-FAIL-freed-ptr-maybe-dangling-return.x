#include <stdlib.h>

int *
func(int x) ~ [
	.alloc result;
	if (x) {
		.dealloc result;	
	}
] {
	int *p;
	p = malloc(1);

	if (x) {
		free(p);	/* p dangling */
	}

	return p;		/* p not may be dangling */
}

int
main()
{
	int *res1;
	int *res2;
	int *i;
	int *j;
	
	i = func(0);
	*i = 1;	/* fine */

	j = func(1);
	*j = 1;	/* ERROR: undefined indirection */

	return 0;
}
