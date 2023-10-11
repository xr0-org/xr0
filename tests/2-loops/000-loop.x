#include <stdlib.h>

void
unit()
{
	int i;
	void *p;

	for (i = 0; i != 9; i++) {
		p = malloc(1);
		[ @p; ]
		free(p);
	}
}
