#include <stdlib.h>

void
dangling_assign(int *i) ~ [
	/* ERROR: returns reference to invalid stack location */
] {
	int p;
	
	p = 5;
	i = &p;		/* some state tracking of pointer to stack allocated var? */

	/* could call some other stuff with the address of p */

	return;		/* after this point i is dangling */
}
