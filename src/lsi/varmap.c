#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "lsi.h"
#include "util.h"

#include "varmap.h"

struct lsi_varmap { struct map *_, *alias; };

static struct lsi_varmap *
_lsi_varmap_create(struct map *m, struct map *alias)
{
	struct lsi_varmap *lv = malloc(sizeof(struct lsi_varmap));
	assert(lv);
	lv->_ = m;
	lv->alias = alias;
	return lv;
}

struct lsi_varmap *
lsi_varmap_create(void)
{
	return _lsi_varmap_create(map_create(), map_create());
}

static char *
_concat(const char *s, const char *t);

struct lsi_varmap *
lsi_varmap_prefix(struct lsi_varmap *lv_old, char *key, char *val)
{
	int i;

	struct map *new = map_create();
	struct map *m = lv_old->_;
	for (i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(new, _concat(key, e.key), _concat(val, e.value));
	}

	struct map *new_alias = map_create();
	m = lv_old->alias;
	for (i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		/* aliases aren't prefixed, only their keys, which denote values */
		map_set(new_alias, _concat(val, e.key), dynamic_str(e.value));
	}

	return _lsi_varmap_create(new, new_alias);
}

static char *
_concat(const char *s, const char *t)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s%s", s, t);
	return strbuilder_build(b);
}

struct lsi_varmap *
lsi_varmap_copy(struct lsi_varmap *lv_old)
{
	int i;

	struct map *new = map_create();
	struct map *m = lv_old->_;
	for (i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(new, dynamic_str(e.key), dynamic_str(e.value));
	}

	struct map *new_alias = map_create();
	m = lv_old->alias;
	for (i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(new_alias, dynamic_str(e.key), dynamic_str(e.value));
	}

	return _lsi_varmap_create(new, new_alias);
}

void
lsi_varmap_destroy(struct lsi_varmap *m)
{
	map_destroy(m->_);
	map_destroy(m->alias);
	free(m);
}

char *
lsi_varmap_str(struct lsi_varmap *lv)
{
	int i;
	struct map *m = lv->_;

	struct strbuilder *b = strbuilder_create();
	strbuilder_putc(b, '{');
	for (i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		strbuilder_printf(
			b, "%s: %s%s", e.key, e.value, i+1<m->n? ", " : ""
		);
	}
	strbuilder_putc(b, '}');
	return strbuilder_build(b);
}

void
lsi_varmap_set(struct lsi_varmap *m, char *k, char *v)
{
	map_set(m->_, k, v);
}

int
_lsi_varmap_haskey(struct lsi_varmap *m, char *k)
{
	return map_get(m->_, k) != NULL;
}

char *
_lsi_varmap_get(struct lsi_varmap *m, char *k)
{
	assert(_lsi_varmap_haskey(m, k));
	return map_get(m->_, k);
}

void
lsi_varmap_addrange(struct lsi_varmap *lv, struct lsi_varmap *lv0)
{
	int i;

	struct map *m  = lv->_,
		   *m0 = lv0->_;

	for (i = 0; i < m0->n; i++) {
		struct entry e = m0->entry[i];
		char *id = map_get(m, e.key);
		if (id) {
			assert(strcmp(id, e.value) == 0);
		} else {
			map_set(m, dynamic_str(e.key), dynamic_str(e.value));
		}
	}

	struct map *alias  = lv->alias,
		   *alias0 = lv0->alias;

	for (i = 0; i < alias0->n; i++) {
		struct entry e = alias0->entry[i];
		char *id = map_get(alias, e.key);
		if (id) {
			assert(strcmp(id, e.value) == 0);
		} else {
			map_set(alias, dynamic_str(e.key), dynamic_str(e.value));
		}
	}
}

void
lsi_varmap_setvaluealias(struct lsi_varmap *lv, char *k, char *alias)
{
	map_set(lv->alias, k, alias);
}

struct lsi_varmap *
lsi_varmap_valuealias_map(struct lsi_varmap *lv_old)
{
	int i;

	struct map *new = map_create();
	struct map *m = lv_old->alias;
	for (i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(new, dynamic_str(e.key), dynamic_str(e.value));
	}
	return _lsi_varmap_create(new, map_create());
}
