#include <stdlib.h>

void *
leak()
{
	void *p;

	p = malloc(1);
	if (p) {
		free(p);
	} else {
		p = malloc(1);
	}
	return p;
}
