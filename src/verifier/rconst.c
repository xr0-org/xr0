#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "lsi.h"
#include "util.h"
#include "value.h"
#include "_limits.h"

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

DEFINE_RESULT_TYPE(char *, str, free, str_res, false)

static char *
rconst_id(struct map *varmap, struct map *persistmap, bool persist);

static struct lsi_expr *
_range_lw(struct ast_expr *range, struct state *);

static struct lsi_expr *
_range_up(struct ast_expr *range, struct state *);

struct str_res *
rconst_declarenokey(struct rconst *v, struct ast_expr *range, bool persist,
		struct state *state)
{
	struct error *err;

	struct map *m = v->varmap;
	char *s = rconst_id(m, v->persist, persist);
	map_set(m, dynamic_str(s), (void *) 1);
	map_set(v->persist, dynamic_str(s), (void *) persist);

	err = lsi_add(
		v->constraints,
		lsi_le_create(
			_range_lw(ast_expr_range_lw(range), state),
			lsi_expr_var_create(dynamic_str(s))
		)
	);
	if (err) {
		return str_res_error_create(err);
	}
	err = lsi_add(
		v->constraints,
		lsi_le_create(
			lsi_expr_var_create(dynamic_str(s)),
			_range_up(ast_expr_range_up(range), state)
		)
	);
	if (err) {
		return str_res_error_create(err);
	}

	return str_res_str_create(s);
}

static struct ast_expr *
_value_to_expr(struct value *, struct state *);

static struct lsi_expr *
_range_lw(struct ast_expr *e, struct state *s)
{
	if (ast_expr_israngemin(e)) {
		return lsi_expr_const_create(C89_INT_MIN);
	} else {
		struct value *v = value_res_as_value(
			eval_to_value(e_res_as_eval(ast_expr_eval(e, s)), s)
		);
		return ast_expr_to_lsi(_value_to_expr(v, s));
	}
}

static struct ast_expr *
_value_to_expr(struct value *v, struct state *s)
{
	return value_isconstant(v)
		? ast_expr_constant_create(value_as_int(v, s))
		: ast_expr_copy(value_as_rconst(v));
}

static struct lsi_expr *
_range_up(struct ast_expr *e, struct state *s)
{
	if (ast_expr_israngemax(e)) {
		return lsi_expr_const_create(C89_INT_MAX);
	} else {
		struct value *v = value_res_as_value(
			eval_to_value(e_res_as_eval(ast_expr_eval(e, s)), s)
		);
		/* subtract 1 b/c range expression upper bounds are exclusive */
		return ast_expr_to_lsi(
			ast_expr_sum_create(
				_value_to_expr(v, s),
				ast_expr_constant_create(-1)
			)
		);
	}
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

struct str_res *
rconst_declare(struct rconst *v, struct ast_expr *range, char *key, bool persist,
		struct state *state)
{
	struct str_res *res = rconst_declarenokey(v, range, persist, state);
	if (str_res_iserror(res)) {
		return res;
	}
	char *s = str_res_as_str(res);
	assert(key);
	map_set(v->keymap, dynamic_str(s), dynamic_str(key));
	return res;
}

int
rconst_hasvar(struct rconst *r, char *var)
{
	return map_get(r->varmap, var) != NULL;
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

struct error *
rconst_constraintverify(struct rconst *spec, struct rconst *impl,
		struct lsi_varmap *m)
{
	struct lsi *spec_lsi = lsi_copy(spec->constraints);
	struct lsi *impl_lsi = lsi_renamevars(impl->constraints, m);
	struct error *err = lsi_addrange(spec_lsi, impl_lsi);
	lsi_destroy(impl_lsi);
	lsi_destroy(spec_lsi);
	return err;
}
