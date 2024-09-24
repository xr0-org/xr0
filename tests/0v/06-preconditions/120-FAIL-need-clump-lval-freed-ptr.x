#include <stdlib.h>

void
func(int *x) ~ [ 
	setup: x = .clump(1);
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
