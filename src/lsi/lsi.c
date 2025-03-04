#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "arr.h"
#include "le.h"

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

static struct string_arr *
_getvars(struct le_arr *);

struct error *
lsi_add(struct lsi *lsi, struct lsi_le *le)
{
	le_arr_append(lsi->arr, le);

	/* TODO: verify feasibility */
	struct string_arr *vars = _getvars(lsi->arr);
	struct strbuilder *b = strbuilder_create();
	int len = string_arr_n(vars);
	char **s = string_arr_s(vars);
	for (int i = 0; i < len; i++) {
		strbuilder_printf(b, "%s%s", s[i], i+1 < len ? ", " : "");
	}
	printf("vars: %s\n", strbuilder_build(b));

	return NULL;
}

static struct map *
_varstomap(struct le_arr *arr);

static struct string_arr *
_getvars(struct le_arr *arr)
{
	int i;

	struct map *m = _varstomap(arr);
	struct string_arr *vararr = string_arr_create();
	for (i = 0; i < m->n; i++) {
		string_arr_append(vararr, dynamic_str(m->entry[i].key));
	}
	map_destroy(m);
	return vararr;
}

static struct map *
_varstomap(struct le_arr *arr)
{
	int i;

	struct map *m = map_create();
	for (i = 0; i < le_arr_len(arr); i++) {
		int j;

		struct string_arr *vararr = _lsi_le_getvars(le_arr_get(arr, i));
		for (j = 0; j < string_arr_n(vararr); j++) {
			char *var = string_arr_s(vararr)[j];
			map_set(m, dynamic_str(var), (void *) 1);
		}
		string_arr_destroy(vararr);
	}
	return m;
}
