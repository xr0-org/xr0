#include <stdlib.h>

/* .x specific test */

void *
allocating() ~ [ .free(result); ];

int
main()
{
	void *p;

	p = allocating();
	free(p);
}

void *
allocating() ~ [ return .malloc(1); ]
{
	void *p;

	p = malloc(1);
	return p;
}
