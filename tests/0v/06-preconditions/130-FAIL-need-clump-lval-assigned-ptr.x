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
	p = 5;
	
	func(p);	/* ERROR: spec required lvalue */
}
