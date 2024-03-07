#include <stdlib.h>

void
dangling_assign(int **i) ~ [
	int p;

	pre: .clump i;

	/* alleged state needs to capture that i gets an address of local var p */
	p = 5;
 	*i = &p;
] {
	int p;

	p = 5;
	*i = &p;

	/* after this point i is dangling */
}

int
main()
{
	int *p;
	p = malloc(1);

	dangling_assign(&p);
	q = *p;			/* ERROR: unjustified dereference */
}
