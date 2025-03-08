#include <stdlib.h>

void
unit(int limit) ~ [ setup: limit = [0?50]; ]
{
	int i;

	for (i = 0; i < limit; i++) ~ [ i = [?]; i = [1?limit+1]; ]
		;

	~ [ i == limit; ]
}
