#include <stdlib.h>

void
uninitialised_memory0()
{
	int p;
	int q;

	q = p;
}

void
uninitialised_memory1()
{
	int *p;
	int *q;
	
	q = *p;
}

void
uninitialised_memory2()
{
	int p;
	int *q;
	int r;

	q = &p;
	r = *q;
}

void
uninitialised_memory3()
{
	int *p;
	int *q;
	p = malloc(5);
	
	q = *p;		/* ERROR: cannot dereference wild pointer */
}
