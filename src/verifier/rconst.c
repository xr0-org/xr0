#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "lsi.h"
#include "util.h"
#include "value.h"
#include "verifier.h"

struct rconst {
	struct map *varmap;
	struct map *keymap;
	struct map *persist;

	struct lsi *constraints;
};

struct rconst *
rconst_create(void)
{
	struct rconst *v = malloc(sizeof(struct rconst));
	v->varmap = map_create();
	v->keymap = map_create();
	v->persist = map_create();
	v->constraints = lsi_create();
	return v;
}

void
rconst_destroy(struct rconst *v)
{
	struct map *m = v->varmap;
	map_destroy(m);
	map_destroy(v->keymap);
	map_destroy(v->persist);
	lsi_destroy(v->constraints);
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
			(void *) 1
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
	new->constraints = lsi_copy(old->constraints);
	return new;
}

struct rconst *
rconst_split(struct rconst *old, struct lsi_le *le)
{
	struct rconst *new = rconst_copy(old);
	struct error *err = lsi_add(new->constraints, le);
	assert(!err); /* origin of split instruct checks feasibility */
	return new;
}


DEFINE_RESULT_TYPE(char *, str, free, str_res, false)

static char *
rconst_id(struct map *varmap, struct map *persistmap, bool persist);

char *
rconst_declarenokey(struct rconst *v, bool persist)
{
	struct map *m = v->varmap;
	char *s = rconst_id(m, v->persist, persist);
	map_set(m, dynamic_str(s), (void *) 1);
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

static char *
rconst_getidbykey(struct rconst *v, char *key);

char *
rconst_declareorget(struct rconst *v, char *key, bool persist)
{
	char *prev = rconst_getidbykey(v, key);
	if (prev) {
		return dynamic_str(prev);
	}
	char *s = rconst_declarenokey(v, persist);
	assert(key);
	map_set(v->keymap, dynamic_str(s), dynamic_str(key));
	return s;
}

int
rconst_hasvar(struct rconst *r, char *var)
{
	return map_get(r->varmap, var) != NULL;
}

static char *
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

struct error *
rconst_addconstraint(struct rconst *v, struct lsi_le *le)
{
	return lsi_add(v->constraints, le);
}

struct str_res *
rconst_getwithconstvalue(struct rconst *v, int c)
{
	struct map *m = v->varmap;
	for (int i = 0; i < m->n; i++) {
		char *var = m->entry[i].key;
		if (lsi_var_isconst(v->constraints, var, c)) {
			return str_res_str_create(var);
		}
	}
	return str_res_error_create(error_printf("none found"));
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
			(void *) 1
		);
		char *c = map_get(v->keymap, key);
		map_set(keymap, dynamic_str(key), dynamic_str(c));
		map_set(persist, dynamic_str(key), (void *) true);
	}

	v->varmap = varmap;
	v->keymap = keymap;
	v->persist = persist;
}

static char *
_constraints_prefix(char *indent);

char *
rconst_str(struct rconst *v, char *indent)
{
	struct strbuilder *b = strbuilder_create();
	struct map *m = v->varmap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		strbuilder_printf(b, "%s%s", indent, e.key);
		char *key = map_get(v->keymap, e.key);
		if (key) {
			strbuilder_printf(b, "\t\"%s\"", key);
		}
		strbuilder_printf(b, "\n");
	}

	char *prefix = _constraints_prefix(indent);
	char *lsi = lsi_str(v->constraints, prefix);
	if (strlen(lsi) > 0) {
		strbuilder_printf(b, "\n%s", lsi);
	}
	free(lsi);
	free(prefix);

	return strbuilder_build(b);
}

static char *
_constraints_prefix(char *indent)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s|- ", indent);
	return strbuilder_build(b);
}

bool
rconst_eval(struct rconst *v, struct ast_expr *e)
{
	return ast_expr_matheval(e);
}

static struct lsi *
_eliminate_rename(struct lsi *, struct lsi_varmap *);

struct error *
rconst_constraintverify(struct rconst *spec, struct rconst *impl,
		struct lsi_varmap *spec_m, struct lsi_varmap *impl_m)
{
	struct lsi *spec_lsi = _eliminate_rename(spec->constraints, spec_m),
		   *impl_lsi = _eliminate_rename(impl->constraints, impl_m);
	struct error *err = lsi_checksatisfiesrange(impl_lsi, spec_lsi);
	lsi_destroy(impl_lsi);
	lsi_destroy(spec_lsi);
	return err;
}

static struct lsi *
_eliminate_rename(struct lsi *lsi, struct lsi_varmap *lv)
{
	struct string_arr *arr = lsi_varmap_keys(lv);
	struct lsi *eliminated = lsi_eliminate_except(lsi, arr);
	string_arr_destroy(arr);
	struct lsi *renamed = lsi_renamevars(eliminated, lv);
	lsi_destroy(eliminated);
	return renamed;
}

int
rconst_satisfies(struct rconst *v, struct lsi_le *le)
{
	return lsi_satisfies(v->constraints, le);
}

struct lsi_range *
rconst_range_eval(struct rconst *v, struct lsi_expr *e)
{
	return lsi_range_eval(v->constraints, e);
}

int
rconst_isfeasible(struct rconst *r, struct lsi_le *le)
{
	struct lsi *constraints = lsi_copy(r->constraints);
	/* XXX: copy leaks when there's an error */
	struct error *err = lsi_add(constraints, lsi_le_copy(le));
	lsi_destroy(constraints);
	if (err) {
		assert(error_to_lsi_notfeasible(err));
		return 0;
	}
	return 1;
}

int
rconst_isanyint(struct rconst *v, char *rconst)
{
	return lsi_var_isanyint(v->constraints, rconst);
}
