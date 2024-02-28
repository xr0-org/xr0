#include <stdlib.h>

int *
dangling_return(int *i) ~ [
	/* ERROR: cannot return dangling stack ptr without static */
] {
	int p;
	
	p = 5;
	return &p;	/* returning dangling pointer */
}
