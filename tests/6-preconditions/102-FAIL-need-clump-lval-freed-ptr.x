#include <stdlib.h>

int
func(int *x) ~ [ 
	pre: x = .clump(1);
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
