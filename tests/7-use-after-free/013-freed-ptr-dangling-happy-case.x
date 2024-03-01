#include <stdlib.h>

int *
func() ~ [
	.alloc result;
	$result;
] {
	int *p;
	p = malloc(1);

	return p;	/* return dangling ptr */
}

int
main()
{
	int *i;
	int *j;

	i = func();
	j = *i;

	return 0;
}
