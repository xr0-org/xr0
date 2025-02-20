#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "state.h"
#include "verifier.h"

struct splitinstruct {
	int n;
	struct map **m;
	struct state *s;
};

struct splitinstruct *
splitinstruct_create(struct state *state)
{
	struct splitinstruct *s = malloc(sizeof(struct splitinstruct));
	assert(s);
	s->n = 0;
	s->m = NULL;
	s->s = state;
	return s;
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


struct state *
splitinstruct_state(struct splitinstruct *s)
{
	return s->s;
}
