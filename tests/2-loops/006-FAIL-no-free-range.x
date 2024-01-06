#include <stdlib.h>

void *
unit() ~ [ .alloc result; ]
{
	int i;
	void **p;

	p = malloc(5);

	for (i = 0; i != 5; i++) ~ [ .alloc p[i]; ] {
		p[i] = malloc(1);
	}

	return p;
}
