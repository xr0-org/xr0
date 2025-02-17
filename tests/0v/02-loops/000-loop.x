#include <stdlib.h>

void
unit()
{
	int i;

	i = 0;

	for (i = 0; i < 9; i++) ~ [ i = [0?10]; ]
		;

	~ [ i == 9; ]
}
