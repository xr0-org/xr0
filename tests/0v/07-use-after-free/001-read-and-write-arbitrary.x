#include <stdlib.h>

#ifdef XR0

int *
read_and_write_arbitrary() ~ [
	int *p;
	p = .malloc(sizeof(int));
	*p = 1;
	return p;
];

#endif

int *
read_and_write_arbitrary()
{
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

	#ifdef XR0
	~ [ *r == 2; ];
	#endif

	free(r);

	return 0;
}
