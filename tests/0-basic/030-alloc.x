#include <stdlib.h>

void *
unit() ~ [ result = .alloc(1); ]
{
	void *p;

	p = malloc(1);
	~ [ @p; ]
	return p;
}
