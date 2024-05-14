#include <stdlib.h>

struct composite {
	void *p;
};

void
allocsub(struct composite *p) ~ [
	setup: p = .clump(1);
	p->p = malloc(1);
]{
	p->p = malloc(1);
}

int
f()
{
	struct composite *c;

	c = malloc(sizeof(struct composite));
	c->p = malloc(1);
	allocsub(c); /* leaks region addressed by c->p */
	free(c->p);
	free(c);
}
