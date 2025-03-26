#include <stdlib.h>

void
unit()
{
	int i, k;
	int *arr;

	arr = malloc(1);

	/* the invariant here asserts the invariance of arr as a block of memory
	 * that may be uninitialised */
	for (i = 0; i < 1; i++) ~ [ i = [0?2]; ]
		arr[i] = 1;

	k = arr[0]; /* may be uninitialised */

	free(arr);
}
