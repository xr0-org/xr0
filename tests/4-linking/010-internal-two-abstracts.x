#include <stdlib.h>

void *
allocating() ~ [ .alloc result; ];

int
main()
{
	void *p;

	p = allocating();
	free(p);
}

void *
allocating() ~ [ .alloc result; ]
{
	void *p;

	p = malloc(1);
	return p;
}
