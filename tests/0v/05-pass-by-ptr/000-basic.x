#include <stdlib.h>

void
assign(void *p) ~ [
	setup: p = .clump(1);
	*p = 1;
]{
	/* TODO: internal verification */
	*p = 1;
}

int
main()
{
	int p; int q;

	assign(&q);
	p = 2;	

	~ [ q == 1; p == 2; ]
	return 0;
}
