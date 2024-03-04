#include <stdlib.h>

int *
dangling_static_return() ~ [
	*result = $;
] {
	static int p;
	
	p = 5;
	return &p;	/* fine since p is declared as static */
}

int
main()
{
	int *q;
	q = dangling_static_return();	/* fine since static variable */
}
