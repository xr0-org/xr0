#include <stdlib.h>

void
leak()
{
	malloc(1);
}
