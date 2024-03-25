#include <stdlib.h>

void
unit(void *p) ~ [
	pre: p = .alloc(1);

	.dealloc(p);
]{
	free(p);
}
