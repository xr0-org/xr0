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
allocating()
{
	void *p;

	p = malloc(1);
	return p;
}
