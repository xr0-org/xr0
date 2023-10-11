#include <stdlib.h>

void
unit(int K)
{
	int i;
	void *p;

	for (i = 0; i != K; i++) {
		p = malloc(1);
		[ @p; ]
		free(p);
	}
}
