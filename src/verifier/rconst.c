#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "lsi.h"
#include "util.h"
#include "value.h"

struct rconst {
	struct map *varmap;
	struct map *keymap;
	struct map *persist;

	struct le_arr *constraints;
};

struct rconst *
rconst_create(void)
{
	struct rconst *v = malloc(sizeof(struct rconst));
	v->varmap = map_create();
	v->keymap = map_create();
	v->persist = map_create();
	v->constraints = le_arr_create();
	return v;
}

void
rconst_destroy(struct rconst *v)
{
	struct map *m = v->varmap;
	for (int i = 0; i < m->n; i++) {
		value_destroy((struct value *) m->entry[i].value);
	}
	map_destroy(m);
	map_destroy(v->keymap);
	map_destroy(v->persist);
	le_arr_destroy(v->constraints);
	free(v);
}

struct rconst *
rconst_copy(struct rconst *old)
{
	struct rconst *new = rconst_create();
	struct map *m = old->varmap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(
			new->varmap,
			dynamic_str(e.key),
			value_copy((struct value *) e.value)
		);
	}
	m = old->keymap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(
			new->keymap,
			dynamic_str(e.key),
			dynamic_str(e.value)
		);
	}
	m = old->persist;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(
			new->persist,
			dynamic_str(e.key),
			e.value
		);
	}
	new->constraints = le_arr_copy(old->constraints);
	return new;
}

static char *
rconst_id(struct map *varmap, struct map *persistmap, bool persist);

char *
rconst_declarenokey(struct rconst *v, struct value *val, bool persist)
{
	struct map *m = v->varmap;
	char *s = rconst_id(m, v->persist, persist);
	map_set(m, dynamic_str(s), val);
	map_set(v->persist, dynamic_str(s), (void *) persist);
	return s;
}

static int
count_true(struct map *m);

static char *
rconst_id(struct map *varmap, struct map *persistmap, bool persist)
{
	int npersist = count_true(persistmap),
	    nnonpersist = varmap->n - npersist;
	struct strbuilder *b = strbuilder_create();
	if (persist) {
		strbuilder_printf(b, "$%d", (int) npersist);
	} else {
		strbuilder_printf(b, "#%d", (int) nnonpersist);
	}
	return strbuilder_build(b);
}

static int
count_true(struct map *m)
{
	int n = 0;
	for (int i = 0; i < m->n; i++) {
		if (m->entry[i].value) {
			n++;
		}
	}
	return n;
}

char *
rconst_declare(struct rconst *v, struct value *val, char *key, bool persist)
{
	char *s = rconst_declarenokey(v, val, persist);
	assert(key);
	map_set(v->keymap, dynamic_str(s), dynamic_str(key));
	return s;
}

struct value *
rconst_get(struct rconst *v, char *id)
{
	return map_get(v->varmap, id);
}

char *
rconst_getidbykey(struct rconst *v, char *key)
{
	/* XXX */
	struct map *m = v->keymap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		char *id = e.key,
		     *varkey = (char *) e.value;
		if (strcmp(key, varkey) == 0) {
			return id;
		}
	}
	return NULL;
}

void
rconst_undeclare(struct rconst *v)
{
	struct map *varmap = map_create(),
		   *keymap = map_create(),
		   *persist = map_create();

	struct map *m = v->varmap;
	for (int i = 0; i < m->n; i++) {
		char *key = m->entry[i].key;
		if (!map_get(v->persist, key)) {
			continue;
		}
		map_set(
			varmap, dynamic_str(key),
			value_copy((struct value *) map_get(v->varmap, key))
		);
		char *c = map_get(v->keymap, key);
		map_set(keymap, dynamic_str(key), dynamic_str(c));
		map_set(persist, dynamic_str(key), (void *) true);
	}

	v->varmap = varmap;
	v->keymap = keymap;
	v->persist = persist;
}

char *
rconst_str(struct rconst *v, char *indent)
{
	struct strbuilder *b = strbuilder_create();
	struct map *m = v->varmap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		char *value = value_str((struct value *) e.value);
		strbuilder_printf(b, "%s%s: %s", indent, e.key, value);
		char *key = map_get(v->keymap, e.key);
		if (key) {
			strbuilder_printf(b, "\t\"%s\"", key);
		}
		strbuilder_printf(b, "\n");
		free(value);
	}

	int len = le_arr_len(v->constraints);
	for (int i = 0; i < len; i++) {
		char *s = le_str(le_arr_get(v->constraints, i));
		strbuilder_printf(b, "%s|- %s\n", indent, s);
		free(s);
	}

	return strbuilder_build(b);
}

bool
rconst_eval(struct rconst *v, struct ast_expr *e)
{
	return ast_expr_matheval(e);
}
