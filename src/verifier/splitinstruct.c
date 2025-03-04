#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "state.h"
#include "verifier.h"

struct splitinstruct {
	struct lsi_le *le0, *le1;
};

struct splitinstruct *
splitinstruct_create(struct lsi_le *le0, struct lsi_le *le1)
{
	struct splitinstruct *s = malloc(sizeof(struct splitinstruct));
	assert(s);
	s->le0 = le0;
	s->le1 = le1;
	return s;
}

struct lsi_le *
splitinstruct_0(struct splitinstruct *s)
{
	return s->le0;
}

struct lsi_le *
splitinstruct_1(struct splitinstruct *s)
{
	return s->le1;
}
