#include <stdlib.h>

void
unit()
{
	int i = 0;
	while (i < 9) {
		i++;
	}
	~ [ i == 9; ] /* unknowable */
}
