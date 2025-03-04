#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "arr.h"

struct lsi { struct le_arr *arr; };

struct lsi *
lsi_create(void)
{
	struct lsi *lsi = malloc(sizeof(struct lsi));
	assert(lsi);
	lsi->arr = le_arr_create();
	return lsi;
}

struct lsi *
lsi_copy(struct lsi *old)
{
	struct lsi *new = lsi_create();
	new->arr = le_arr_copy(old->arr);
	return new;
}

void
lsi_destroy(struct lsi *lsi)
{
	le_arr_destroy(lsi->arr);
	free(lsi);
}

char *
lsi_str(struct lsi *lsi, char *prefix)
{
	struct strbuilder *b = strbuilder_create();
	struct le_arr *arr = lsi->arr;
	for (int i = 0; i < le_arr_len(arr); i++) {
		char *s = lsi_le_str(le_arr_get(arr, i));
		strbuilder_printf(b, "%s%s\n", prefix, s);
		free(s);
	}
	return strbuilder_build(b);
}

struct error *
lsi_add(struct lsi *lsi, struct lsi_le *le)
{
	le_arr_append(lsi->arr, le);
	/* TODO: verify feasibility */
	return NULL;
}
