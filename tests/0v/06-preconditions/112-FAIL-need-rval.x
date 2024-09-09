#include <stdlib.h>

int
func(int *x) ~ [ 
	setup: {
		x = .clump(1);
		*x = [?];
	}
	return *x;
]{
	return *x;	
}

int
main()
{
	int p;
	int q;
	q = func(&p);	/* ERROR: spec required rvalue */
}
