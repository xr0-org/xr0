#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "verifier.h"

struct splitinstruct {
	int n;
	struct map **m;
};

struct splitinstruct *
splitinstruct_create(void)
{
	return calloc(1, sizeof(struct splitinstruct));
}

void 
splitinstruct_append(struct splitinstruct *s, struct map *m)
{
	s->m = realloc(s->m, sizeof(struct map *) * ++s->n);
	assert(s->m);
	s->m[s->n-1] = m;
}

int 
splitinstruct_n(struct splitinstruct *s)
{
	return s->n;
}

struct map **
splitinstruct_splits(struct splitinstruct *s)
{
	return s->m;
}
