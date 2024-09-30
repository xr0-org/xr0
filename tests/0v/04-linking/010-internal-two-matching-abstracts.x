#include <stdlib.h>

/* .x specific test */

void *
allocating() ~ [ return .malloc(1); ];

int
main()
{
	void *p;

	p = allocating();
	free(p);
	return 0;
}

void *
allocating() ~ [ return .malloc(1); ]
{
	void *p;

	p = malloc(1);
	return p;
}
