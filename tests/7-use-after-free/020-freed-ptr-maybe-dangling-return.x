#include <stdlib.h>

int
main()
{
	int *res1;
	int *res2;
	int *i;
	int *j;
	
	i = func(0);
	res1 = *i;	/* fine */

	j = func(1);
	res2 = *j;	/* ERROR: unjustified indirection */

	return 0;
}

int *
func(int x) ~ [
	if (x) {
		!$result;	/* not necessary */
	}
	$result;
] {
	int *p;
	p = malloc(1);

	if (x) {
		free(p);	/* p dangling */
	}
	
	return p;		/* some mechanism to indicate p may be dangling */		
}
