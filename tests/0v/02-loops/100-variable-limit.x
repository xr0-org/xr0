#include <stdlib.h>

void
unit(int limit) ~ [ setup: limit = [0?50]; ]
{
	int i;

	for (i = -1; i < limit; i++) ~ [ i = [?]; i = [0?limit+1]; ]
		;

	~ [ i == limit; ]
}
