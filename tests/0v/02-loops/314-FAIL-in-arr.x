#include <stdlib.h>

void
unit()
{
	int i, k;
	int *arr;

	arr = malloc(2);
	arr[0] = 0;
	arr[1] = 1;

	for (i = 0; i < 2; i++) ~ [ i = [0?3]; ]
		arr[i] = 0;

	free(arr);
}
