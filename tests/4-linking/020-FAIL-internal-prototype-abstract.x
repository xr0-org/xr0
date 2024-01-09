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
allocating() /* should have matching abstract */
{
	void *p;

	p = malloc(1);
	return p;
}
