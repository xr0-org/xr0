#include <stdlib.h>

void
dangling_assign(int **i) ~ [
	int j;

	pre: { .clump i; }

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
	int q;

	dangling_assign(&p);
	q = *p;			/* ERROR: undefined dereference */
}
