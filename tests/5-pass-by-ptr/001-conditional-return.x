#include <stdlib.h>

int
select(int *p, int *q, int x) ~ [
	if (x) {
		return p;
	}
	if (!x) {
		return q;
	}
] {
	if (x) {
		return p;
	}
	if (!x) {
		return q;
	}
}

int *
main()
{
	int *p;
	int *q;
	int *r;
	p = malloc(sizeof(int));
	q = malloc(sizeof(int));
	*p = 1;
	*q = 2;
	r = select(p, q, 0);
	~ [ r == 1; ];
	r = select(p, q, 1);
	~ [ r == 2; ];
	free(q);
	free(p);
}
	
