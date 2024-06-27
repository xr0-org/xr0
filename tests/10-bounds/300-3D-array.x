#include <stdlib.h>

int
foo()
{
	int *i;
	int *a[2][2][2];

	i = malloc(1);
	*i = 2; 
	a[1][1][1] = i;
	/* ~ [ *a[1][1][1] == 2; ] */ /* XXX: expand verify */
	free(i);
}
