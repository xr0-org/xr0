#include <stdlib.h>

int
foo()
{
	char c;
	char d;

	char *a[4][3];
	a[0][11] = &c;
	a[1][8] = &d;
	return 0;
}
