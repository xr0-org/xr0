#include <stdlib.h>

int
func(int *x) ~ [ 
	pre: .alloc x;
	*x = 5;
]{
	*x = 5;	
}

int
main()
{
	int p;
	func(&p);	/* ERROR: spec requires heap */
}
