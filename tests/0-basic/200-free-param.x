#include <stdlib.h>

void
unit(void *p) ~ [
	pre: .alloc p;

	.dealloc p;
]{
	free(p);
}
