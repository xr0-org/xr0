#include <stdlib.h>

struct composite {
	void *p;
};

notleak(struct composite *p) ~ [
	setup: p = .clump(1);
	p->p = malloc(1);
]{
	p->p = malloc(1);
	p = NULL;
}
