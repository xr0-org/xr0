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
	p = 5;
	
	func(p);	/* ERROR: spec required lvalue */
}
