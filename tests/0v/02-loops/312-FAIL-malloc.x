#include <stdlib.h>

void
unit()
{
	int i, k;
	int *arr;

	arr = malloc(1);
	arr[0] = 0;

	for (i = 0; i < 1; i++) ~ [ i = [0?2]; ]
		arr[i] = 1; /* invalidates invariant arr[0] = 1 */ 

	k = arr[0];

	free(arr);
}
