#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "lsi.h"
#include "util.h"

#include "varmap.h"

struct lsi_varmap { struct map *_; };

struct lsi_varmap *
lsi_varmap_create(void)
{
	struct lsi_varmap *m = malloc(sizeof(struct lsi_varmap));
	assert(m);
	m->_ = map_create();
	return m;
}

void
lsi_varmap_destroy(struct lsi_varmap *m)
{
	map_destroy(m->_);
	free(m);
}

void
lsi_varmap_set(struct lsi_varmap *m, char *k, char *v)
{
	map_set(m->_, k, v);
}

char *
_lsi_varmap_get(struct lsi_varmap *m, char *k)
{
	char *v = map_get(m->_, k);
	assert(v);
	return v;
}

char *
lsi_varmap_str(struct lsi_varmap *m)
{
	assert(0);
}
