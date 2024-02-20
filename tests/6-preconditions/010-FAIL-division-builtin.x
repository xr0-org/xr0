#include <stdio.h>
#include <stdlib.h>

int
main()
{
	int x;
	int y;
	int r;
	
	puts("Enter x: ");
        scanf("%d", &x);
	puts("Enter y: ");
        scanf("%d", &y);

	r = x/y;

	return r;
}
