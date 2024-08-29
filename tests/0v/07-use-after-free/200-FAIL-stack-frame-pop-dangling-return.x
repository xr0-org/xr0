#include <stdlib.h>

int *
dangling_return()
{
	int p;

	p = 5;
	return &p;	/* returning dangling pointer */
}

int
main()
{
	int *p;
	int q;

	p = dangling_return();
	q = *p;			/* ERROR: undefined dereference */
}
