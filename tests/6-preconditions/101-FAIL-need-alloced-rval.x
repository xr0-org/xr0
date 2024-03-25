#include <stdlib.h>

int
func(int *x) ~ [ 
	int i;
	pre: {
		x = .alloc(1);
		*x = $;
	}
	result = *x;
]{
	return *x;	
}

int
main()
{
	int p;
	int q;
	p = malloc(1);
	q = func(p);	/* ERROR: spec required rvalue */
}
