#include <stdlib.h>

void
unit(void *p) ~ [
	pre: p = .malloc(1);

	.free(p);
]{
	free(p);
}
