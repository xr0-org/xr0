#include <stdlib.h>

void *
unit(int K) [ if (K >= 3) .alloc result; ]
{
	int i;
	void **p;
	void *q;

	if (K < 3) {
		return NULL;
	}

	p = malloc(sizeof(void *) * K);

	for (i = 0; i < K; i++) [ .alloc p[i]; ] {
		p[i] = malloc(sizeof(void *));
	}

	free(p[0]);
	for (i = 1; i < K-2; i++) [ .free p[i]; ] {
		free(p[i]);
	}
	free(p[K-2])

	q = p[K-1];

	free(p);

	return q;
}
