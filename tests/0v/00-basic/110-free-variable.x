#include <stdlib.h>

void
unit()
{
	void *p;

	p = malloc(1);
	if (p) {
		~ [ @p; ]
	}
	free(p);
	~ [ !@p; ]
}
