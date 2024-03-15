#include <stdlib.h>

int
func(int *x) ~ [ 
	pre: .clump x;
	*x = 5;
]{
	*x = 5;	
}

int
main()
{
	int *p;
	func(p);	
}
