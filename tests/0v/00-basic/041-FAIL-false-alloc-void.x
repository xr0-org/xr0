#include <stdlib.h>

void *
unit() ~ [ return .malloc(1); ]
{
	return NULL;
}
