#include <stdlib.h>

void *
unit() ~ [ return .alloc(1); ]
{
	void *p;

	p = malloc(1);
	~ [ @p; ]
	return p;
}
