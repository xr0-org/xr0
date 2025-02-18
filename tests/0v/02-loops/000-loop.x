#include <stdlib.h>

void
unit()
{
	int i;

	for (i = 0; i < 9; i = i + 2) ~ [ i = [0?10]; ]
		;

	~ [ i == 9; ]
}
