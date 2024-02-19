#include <stdio.h>
#include <stdlib.h>

int
ratio(int x, int y) ~ [
	pre: y != 0;
	result = x/y;
]{
	return x/y;
} 

int
main()
{
	/* TODO: declarator lists are truncated, only first declared in state */
	int x;
	int y;
	int r;
	
	/* 11 is Maximum length of a 32 but integer including sign */
	/*char res[11];*/
	char *res;

	res = malloc(11);

	puts("Enter x: ");
        scanf("%d", &x);
	puts("Enter y: ");
        scanf("%d", &y);

	if (y != 0) {
		free(res); 
		/* XXX: should be `exit'. ignore memory concerns for `exit'? */
		return 0;
	}
	
	r = ratio(x, y);

	/* use sprintf to convert int to string */
	sprintf(res, "%d", r);
        puts(res);

	free(res);
	return 1;
}
