#include <stdlib.h>

#ifdef XR0

void
func(int *x) ~ [ 
	setup: x = NULL;
];

#endif

void
func(int *x)
{
	/* empty */	
}

int
main()
{
	int *p;
	p = 5;
	
	func(p);
	return 0;
}
