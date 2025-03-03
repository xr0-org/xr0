#include <stdlib.h>
#include <assert.h>

#include "util.h"

#include "tally.h"

struct tally {
	struct map *m;
	int c;
};

struct tally *
tally_create(void)
{
	struct tally *t = malloc(sizeof(struct tally));
	assert(t);
	t->m = map_create();
	t->c = 0;
	return t;
}

void
tally_destroy(struct tally *t)
{
	map_destroy(t->m);
	free(t);
}

char *
tally_str(struct tally *t)
{
	assert(0);
}

int
tally_getcoef(struct tally *t, char *var)
{
	return (long) map_get(t->m, var);
}

int
tally_getconst(struct tally *t)
{
	return t->c;
}

void
tally_setcoef(struct tally *t, char *var, int c)
{
	map_set(t->m, var, (void *) (long) c);
}

void
tally_setconst(struct tally *t, int c)
{
	t->c = c;
}

struct string_arr;

struct string_arr *
tally_getvars(struct tally *t)
{
	assert(0);
}
