#include <stdlib.h>

struct composite {
	void *p;
};

#ifdef XR0

void
allocsub(struct composite *p) ~ [
	setup: p = .clump(1);
	p->p = malloc(1);
];

#endif

void
allocsub(struct composite *p)
{
	p->p = malloc(1);
}
