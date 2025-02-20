#include <stdlib.h>

void
unit(int limit) ~ [ setup: limit = [0?]; ]
{
	int i;

	for (i = 0; i < limit; i++) ~ [ i = [0?limit+1]; ]
		;

	~ [ i == limit; ]
}
