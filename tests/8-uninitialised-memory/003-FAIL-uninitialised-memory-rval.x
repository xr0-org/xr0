#include <stdlib.h>

void
undefined_memory3()
{
	int *p;
	int *q;
	p = malloc(5);
	
	q = *p;		/* ERROR: cannot dereference wild pointer */
}
