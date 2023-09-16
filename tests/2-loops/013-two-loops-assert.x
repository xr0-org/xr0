#include <stdlib.h>

void
unit()
{
	int i;
	void **p;

	p = malloc(sizeof(void *) * 10);

	for (i = 0; i < 10; i++) [ .alloc p[i]; ] {
		p[i] = malloc(sizeof(void *));
	}

	[ for (i = 0; i < 10; i++) { @p[i]; } ]

	for (i = 0; i < K-1; i++) [ .free p[i]; ] {
		free(p[i]);
	}

	[ for (i = 0; i < 10; i++) { @p[i]; } ]

	free(p);
}
