#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "le.h"
#include "expr_arr.h"
#include "le_arr.h"

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

static struct error *
_verifyfeasible(struct lsi *);

struct error *
lsi_add(struct lsi *lsi, struct lsi_le *le)
{
	le_arr_append(lsi->arr, le);
	return _verifyfeasible(lsi);
}

static struct string_arr *
_getvars(struct le_arr *);

static struct le_arr *
_eliminate(struct le_arr *, char *var);

static struct error *
_verifyfeasible(struct lsi *lsi)
{
	int i;

	struct le_arr *arr = le_arr_copy(lsi->arr);

	struct string_arr *vars = _getvars(lsi->arr); /* TODO: reverse list */
	for (i = 0; i < string_arr_n(vars); i++) {
		arr = _eliminate(arr, string_arr_s(vars)[i]);
	}
	string_arr_destroy(vars);

	for (i = 0; i < le_arr_len(arr); i++) {
		struct lsi_le *le = le_arr_get(arr, i);
		if (!_lsi_le_isfeasible(le)) {
			struct strbuilder *b = strbuilder_create();
			char *s = lsi_le_str(le);
			strbuilder_printf(b, "infeasible system requires %s", s);
			free(s);
			return error_printf(strbuilder_build(b));
		}
	}

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

static struct le_arr *
_eliminate(struct le_arr *old, char *var)
{
	int i, j;

	struct le_arr *new = le_arr_create();

	struct expr_arr *lhs = expr_arr_create(),
			*rhs = expr_arr_create();

	for (i = 0; i < le_arr_len(old); i++) {
		struct lsi_le *le = le_arr_get(old, i);
		int coef = _lsi_le_getstdformcoef(le, var);
		if (coef == 0) {
			le_arr_append(new, _lsi_le_copy(le));
			continue;
		}
		int abs = coef >= 0 ? coef : -coef;
		assert(abs == 1); /* TODO fractions */
		/* coef is the standard-form coefficient, so if it's negative
		 * the inequality appears on the rhs as a positive, and the
		 * inequality provides a lower bound; or vice-versa. */
		if (coef < 0) {
			expr_arr_append(lhs, _lsi_le_lowerbound(le, var));
		} else {
			assert(coef > 0);
			expr_arr_append(rhs, _lsi_le_upperbound(le, var));
		}
	}

	for (i = 0; i < expr_arr_len(lhs); i++) {
		for (j = 0; j < expr_arr_len(rhs); j++) {
			le_arr_append(
				new,
				lsi_le_create(
					expr_arr_get(lhs, i),
					expr_arr_get(rhs, j)
				)
			);
		}
	}

	return new;
}
