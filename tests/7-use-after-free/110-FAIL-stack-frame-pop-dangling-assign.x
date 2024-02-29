#include <stdlib.h>

int
main()
{
	int *p;
	p = malloc(1);

	dangling_assign(&p);
	q = *p;			/* ERROR: unjustified dereference */
}

void
dangling_assign(int **i) ~ [
	pre: $i;
] {
	int p;

	p = 5;
	*i = &p;		/* some state tracking of pointer to stack allocated var? */

	/* could call some other stuff with the address of p */

	/* after this point i is dangling */
}
