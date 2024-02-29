#include <stdlib.h>

int
main()
{
	int *i;
	int *j;

	i = func();
	j = *i;

	return 0;
}

int *
func() ~ [
	.alloc result;
	$result;	/* false claim */
] {
	int *p;
	p = malloc(1);

	return p;	/* return dangling ptr */
}

int *
func() ~ [
	.alloc result;
	$result;	/* false claim */
] {
	int *p;
	p = malloc(1);

	return p;	/* return dangling ptr */
}
