#include <stdlib.h>

void
unit()
{
	int i;
	void **p;

	p = malloc(9);

	for (i = 0; i < 9; i++) [ .alloc p[i]; ] {
		p[i] = malloc(1);
	}

	for (i = 0; i < 9; i++) [ .dealloc p[i]; ] {
		free(p[i]);
	}

	free(p);
}
