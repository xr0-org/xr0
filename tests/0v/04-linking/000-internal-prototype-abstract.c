#include <stdlib.h>

#ifdef XR0

void *
allocating() ~ [ return .malloc(1); ];

#endif

int
main()
{
	void *p;

	p = allocating();
	free(p);
	return 0;
}

void *
allocating()
{
	void *p;

	p = malloc(1);
	return p;
}
