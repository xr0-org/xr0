#include <stdlib.h>

int
foo()
{
	char a[4][3];
	a[3][2] = 7;
}

0: |12| {3:<>} (char[3][4] a)


a[3] = *(a+3)

a[3][2] = *(*(a+3)+2)

a+3 is of type pointer to char[3]
*(a+3) is of type char[3], which as an lvalue will become char *
