#include <stdlib.h>

int *
read_and_write_definite() ~ [ .alloc result; *result = 1; ]
{
	int *p;
	p = malloc(1);
	*p = 1;
	return p;
}

int
main()
{
	int *q;
	int i;

	q = read_and_write_definite();
	i = *q;		/* valid rvalue deref */
	~ [ *q == 1; ];
	~ [ i == 1; ];	/* i has definite value */
	*q = 2;		/* valid lvalue deref */
	~ [ *q == 2; ];

	free(q);
}
