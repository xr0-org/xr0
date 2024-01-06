#include <stdlib.h>

void *
unit() ~ [ .alloc result; ]
{
	int i;
	void **p;
	void *q;
	void *r;

	p = malloc(5);

	for (i = 0; i != 5; i++) ~ [ .alloc p[i]; ] {
		p[i] = malloc(1);
	}
	for (i = 0; i != 3; i++) ~ [ .dealloc p[i]; ] {
		free(p[i]);
	}

	~ [ for (i = 0; i != 5; i++) { !@p[i]; }; ]

	q = p[3];
	r = p[4];

	free(p);

	free(q);

	return r;
}

