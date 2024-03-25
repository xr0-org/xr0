#include <stdlib.h>

void *
allocating() ~ [ return .malloc(1); ];

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
