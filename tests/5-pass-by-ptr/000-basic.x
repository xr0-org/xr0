#include <stdlib.h>

void *
assign(void *p) ~ [
	*p = 1;
] {
	/* TODO: internal verification */
	*p = 1;
}

int
main() {
	int q;
	int p;

	assign(&q);
	p = 2;	

	~ [ q == 1; p == 2; ]
}
