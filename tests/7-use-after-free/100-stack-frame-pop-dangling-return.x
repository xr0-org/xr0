#include <stdlib.h>

int
main()
{
	int *p;
	int q;

	p = dangling_return();
	q = *p;		/* ERROR: unjustified dereference */
}

int *
dangling_return() ~ [

] {
	int p;

	p = 5;
	return &p;	/* returning dangling pointer */
}
