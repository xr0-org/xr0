#include <stdlib.h>

#ifdef XR0

int
func(int *x) ~ [ 
	setup: {
		x = .clump(1);
		*x = [?];
	}
	return *x;
];

#endif

int
func(int *x)
{
	return *x;	
}

int
main()
{
	int p;
	int q;
	q = func(&p);	/* ERROR: spec required rvalue */
}
