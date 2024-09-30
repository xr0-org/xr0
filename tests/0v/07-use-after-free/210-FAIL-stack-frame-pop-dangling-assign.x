#include <stdlib.h>

#ifdef XR0

void
dangling_assign(int **i) ~ [
	int j;

	setup: i = .clump(sizeof(int *));

	j = 5;
 	*i = &j;
];

#endif

void
dangling_assign(int **i)
{
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
