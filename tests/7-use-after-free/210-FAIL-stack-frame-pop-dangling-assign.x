#include <stdlib.h>

void
dangling_assign(int **i) ~ [
	int j;

	pre: .clump i;

	/* alleged state needs to capture that i gets an address of local var p */
	j = 5;
 	*i = &j;
] {
	int j;

	j = 5;
	*i = &j;

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
