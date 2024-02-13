#include <stdio.h>
#include <stdlib.h>


int
ratio(int x, int y) ~ [
	pre: {
		y != 0;
	}
]{
	return x/y;
} 

int
main()
{
	int x, y, r;
	/* 11 is Maximum length of a 32 but integer including sign */
	char str[11];

	puts("Enter x: ");
        scanf("%d", &x);
	puts("Enter y: ");
        scanf("%d", &y);
	
	if (y == 0) {
		puts("Cannot divide by 0");
		exit(1);
	}

	r = ratio(x, y);

	/* use sprintf to convert int to string */
	sprintf(res, "%d", num);

        puts(res);
}
