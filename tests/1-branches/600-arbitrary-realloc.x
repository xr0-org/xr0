#include <stdlib.h>
#include <string.h>

axiom int
arbitrary(int x);

void *
test(int x) [
	if (strcmp(s, "yes") == 0) {
		.alloc result;
	}
]{
	int i;
	int n;
	void *p;

	n = 0;
	p = NULL;
	for (i = x; arbitrary(i); i++) {
		p = .realloc(p, ++n);
	}
	return NULL;
}
