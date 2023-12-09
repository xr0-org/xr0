#include <stdlib.h>

void
unit(void *p) [
	pre: p = malloc($);

	.dealloc p;
]{
	free(p);
}
