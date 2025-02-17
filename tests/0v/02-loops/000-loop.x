#include <stdlib.h>

void
unit()
{
	int i;

	i = 0;

	while (1) ~ [ i = [0?10]; ] {
		if (!(i < 9))
			break;
		i++;
	}

	~ [ i == 9; ]
}
