#include <stdlib.h>

int
func(int *x) ~ [ 
	setup: x = .malloc(1);
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
