#include <stdlib.h>

void *
allocating() ~ [ return .malloc(1); ];

void *
nonallocating();

void *
allocating()
{
	void *p;
	void *q;

	p = malloc(1);
	q = nonallocating();
	return p;
}

void *
nonallocating()
{
	return NULL;
}
