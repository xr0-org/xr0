#include <stdlib.h>

int *
read_and_write_arbitrary() ~ [
	int *p;
	p = .malloc(sizeof(int));
	*p = 1;
	return p;
] {
	int *p;
	p = malloc(sizeof(int));
	*p = 1;
	return p;
}

int
main()
{
	int *r;
	int j;

	r = read_and_write_arbitrary();
	j = *r;		/* valid rvalue deref */
	// ~ [ *r == $; ];
	// ~ [ j == $; ];	/* j has arbitrary value */
	*r = 2;		/* valid lvalue deref */
	~ [ *r == 2; ];

	free(r);

	return 0;
}
