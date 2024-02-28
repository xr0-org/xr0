#include <stdlib.h>

int
func(int *x) ~ [ 
	pre: .clump x;
]{
}

int
main()
{
	int *p;
	p = malloc(1);
	free(p);
	
	func(p);	/* ERROR: spec required rvalue */
}
