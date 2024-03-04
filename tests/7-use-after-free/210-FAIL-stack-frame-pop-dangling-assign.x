#include <stdlib.h>

void
dangling_assign(int **i) ~ [
	pre: $i;
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
