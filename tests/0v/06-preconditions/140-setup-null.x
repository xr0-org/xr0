#include <stdlib.h>

void
func(int *x) ~ [ 
	setup: x = NULL;
]{
	
}

int
main()
{
	int *p;
	p = 5;
	
	func(p);
	return 0;
}
