#include <stdlib.h>

int *
foo(int **arr) ~ [
	setup: {
		arr = .clump(2);
		arr[0] = [?];
		arr[1] = .clump(1);
		arr[1][0] = [?];
	}
	return arr[0];
]{
	int k;
	k = arr[1][0];
	return arr[0];
}

void
bar()
{
	int *obj[2];

	obj[0] = 0;
	obj[1] = malloc(1);
	obj[1][0] = 0;
	foo(obj);
	free(obj[1]);
}
