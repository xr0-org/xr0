#include <stdlib.h>

#ifdef XR0

void
assign(void *p) ~ [
	setup: p = .clump(1);
	*p = 1;
];

#endif

void
assign(void *p)
{
	/* TODO: internal verification */
	*p = 1;
}

int
main()
{
	int p; int q;

	assign(&q);
	p = 2;	

	#ifdef XR0
	~ [ q == 1; p == 2; ]
	#endif

	return 0;
}
