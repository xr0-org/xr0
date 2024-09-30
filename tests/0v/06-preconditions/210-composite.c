#include <stdlib.h>

struct composite {
	void *p;
};


#ifdef XR0

void
notleak(struct composite *p) ~ [
	setup: p = .clump(1);
	p->p = malloc(1);
];

#endif

void
notleak(struct composite *p)
{
	p->p = malloc(1);
}
