#include <stdlib.h>

void
unit()
{
	int i; int j;
	void **p;

	p = malloc(sizeof(void *) * 10);
	for (i = 0; i < 10; i++) [ .alloc p[i]; ] {
		p[i] = malloc(sizeof(void *));
		[ for (j = 0; j < i; j++) { @p[j]; } ]
	}
	for (i = 0; i < 10; i++) [ .free p[i]; ] {
		free(p[i]);
		[ for (j = 0; j < i; j++) { !@p[j]; } ]
	}
	free(p);
}
