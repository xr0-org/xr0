#include <stdlib.h>

void
unit(void *p) ~ [
	setup: p = .malloc(1);

	.free(p);
]{
	free(p);
}
