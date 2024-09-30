#include <stdlib.h>

int
foo()
{
	char c;
	char *a[4][3];
	a[0][0] = &c;
	a[1][7] = a[0][0];
	*a[1][7] = 5;
	/* ~ [ *a[1][7] == 5; ]; */ /*XXX: expand verify */
	return 0;
}
