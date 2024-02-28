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
	p = 5;
	
	func(p);	/* ERROR: spec required lvalue */
}
