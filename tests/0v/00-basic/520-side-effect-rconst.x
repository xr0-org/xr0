#include <stdlib.h>

void
f(int *p) ~ [
	setup: p = malloc(1);
	*p = [?];
]{
	*p = 5;
}
