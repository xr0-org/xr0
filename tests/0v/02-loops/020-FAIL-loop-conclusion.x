#include <stdlib.h>

void
unit()
{
	int i;

	for (i = 0; i < 9; i++) ~ [ i = [0?10]; ]
		;

	~ [ 8 <= i; i <= 8; ]
}
