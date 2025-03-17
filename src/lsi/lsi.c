#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "util.h"
#include "lsi.h"
#include "_limits.h"

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

struct lsi *
lsi_renamevars(struct lsi *old, struct lsi_varmap *m)
{
	int i;

	struct lsi *new = lsi_create();
	for (i = 0; i < le_arr_len(old->arr); i++) {
		le_arr_append(
			new->arr, _lsi_le_renamevars(le_arr_get(old->arr, i), m)
		);
	}
	return new;
}

struct lsi *
lsi_prefixvars(struct lsi *old, char *prefix)
{
	int i;

	struct lsi *new = lsi_create();
	for (i = 0; i < le_arr_len(old->arr); i++) {
		le_arr_append(
			new->arr,
			_lsi_le_prefixvars(le_arr_get(old->arr, i), prefix)
		);
	}
	return new;

}

static struct le_arr *
_eliminate_except(struct le_arr *, struct string_arr *);

static struct le_arr *
_eliminate_trivimpl(struct le_arr *);

struct lsi *
lsi_eliminate_except(struct lsi *old, struct string_arr *arr)
{
	struct lsi *new = lsi_create();
	le_arr_destroy(new->arr);
	struct le_arr *temp = _eliminate_except(old->arr, arr);
	new->arr = _eliminate_trivimpl(temp);
	le_arr_destroy(temp);
	return new;
}

static struct string_arr *
_getvars_except(struct le_arr *, struct string_arr *);

static struct le_arr *
_eliminate(struct le_arr *, char *var);

static struct le_arr *
_eliminate_except(struct le_arr *old, struct string_arr *incl)
{
	int i;

	struct le_arr *new = le_arr_copy(old);

	struct string_arr *elim = _getvars_except(new, incl);
	for (i = 0; i < string_arr_n(elim); i++) {
		struct le_arr *temp = new;
		new = _eliminate(temp, string_arr_s(elim)[i]);
		le_arr_destroy(temp);
	}
	string_arr_destroy(elim);

	return new;
}

static struct string_arr *
_getvars(struct le_arr *);

static int
_in(struct string_arr *, char *);

static struct string_arr *
_getvars_except(struct le_arr *arr, struct string_arr *incl)
{
	int i;

	struct string_arr *exc = string_arr_create();

	struct string_arr *vars = _getvars(arr);
	for (i = 0; i < string_arr_n(vars); i++) {
		char *var = string_arr_s(vars)[i];
		if (!_in(incl, var))
			string_arr_append(exc, dynamic_str(var));
	}
	string_arr_destroy(vars);

	return exc;
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

static int
_in(struct string_arr *arr, char *s)
{
	int i;

	for (i = 0; i < string_arr_n(arr); i++)
		if (strcmp(s, string_arr_s(arr)[i]) == 0)
			return 1;

	return 0;
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
			le_arr_append(new, lsi_le_copy(le));
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

	for (i = 0; i < expr_arr_len(lhs); i++)
		for (j = 0; j < expr_arr_len(rhs); j++) {
			struct lsi_le *le = lsi_le_create(
				expr_arr_get(lhs, i), expr_arr_get(rhs, j)
			);
			struct lsi_le *zero = lsi_le_create(
				lsi_expr_const_create(0),
				lsi_expr_const_create(0)
			);
			if (!_lsi_le_eq(le, zero))
				le_arr_append(new, lsi_le_copy(le));
			lsi_le_destroy(zero);
			lsi_le_destroy(le);
		}

	return new;
}

static int
_trivimpl_by_another(struct le_arr *, struct lsi_le *);

static struct le_arr *
_eliminate_trivimpl(struct le_arr *old)
{
	int i;

	struct le_arr *new = le_arr_create();
	for (i = 0; i < le_arr_len(old); i++) {
		struct lsi_le *le = le_arr_get(old, i);
		if (!_trivimpl_by_another(old, le))
			le_arr_append(new, lsi_le_copy(le));
	}

	return new;
}

static int
_trivimpl_by_another(struct le_arr *arr, struct lsi_le *le)
{
	int i;

	for (i = 0; i < le_arr_len(arr); i++) {
		struct lsi_le *other = le_arr_get(arr, i);
		if (!_lsi_le_eq(other, le) && _lsi_le_trivimpl(other, le))
			return 1;
	}

	return 0;
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

static int
_var_isconstlowerbounded(struct le_arr *, char *var, int c);

static int
_var_isconstupperbounded(struct le_arr *, char *var, int c);

int
lsi_var_isconst(struct lsi *lsi, char *var, int c)
{
	struct le_arr *arr = lsi->arr;
	return _var_isconstlowerbounded(arr, var, c)
		&& _var_isconstupperbounded(arr, var, c);
}

static int
_var_isconstlowerbounded(struct le_arr *arr, char *var, int c)
{
	for (int i = 0; i < le_arr_len(arr); i++) {
		if (_lsi_le_isconstlowerbound(le_arr_get(arr, i), var, c)) {
			return 1;
		}
	}
	return 0;
}

static int
_var_isconstupperbounded(struct le_arr *arr, char *var, int c)
{
	for (int i = 0; i < le_arr_len(arr); i++) {
		if (_lsi_le_isconstupperbound(le_arr_get(arr, i), var, c)) {
			return 1;
		}
	}
	return 0;
}

int
lsi_var_isanyint(struct lsi *lsi, char *var)
{
	struct le_arr *arr = lsi->arr;
	return _var_isconstlowerbounded(arr, var, C89_INT_MIN)
		&& _var_isconstupperbounded(arr, var, C89_INT_MAX);
}

static int
_trivimpl(struct le_arr *, struct lsi_le *);

static struct error *
_verifyfeasible(struct lsi *);

struct error *
lsi_add(struct lsi *lsi, struct lsi_le *le)
{
	if (!_trivimpl(lsi->arr, le)) {
		le_arr_append(lsi->arr, le);
		return _verifyfeasible(lsi);
	}
	return NULL;
}

static int
_trivimpl(struct le_arr *arr, struct lsi_le *le)
{
	int i;

	for (i = 0; i < le_arr_len(arr); i++)
		if (_lsi_le_trivimpl(le_arr_get(arr, i), le))
			return 1;

	return 0;
}

static struct error *
_verifyfeasible(struct lsi *lsi)
{
	int i;

	struct le_arr *arr = le_arr_copy(lsi->arr);

	struct string_arr *vars = _getvars(lsi->arr); /* TODO: reverse list */
	for (i = 0; i < string_arr_n(vars); i++) {
		struct le_arr *temp = arr;
		arr = _eliminate(temp, string_arr_s(vars)[i]);
		le_arr_destroy(temp);
	}
	string_arr_destroy(vars);

	for (i = 0; i < le_arr_len(arr); i++) {
		struct lsi_le *le = le_arr_get(arr, i);
		if (!_lsi_le_isfeasible(le)) {
			return error_lsi_notfeasible();
		}
	}

	return NULL;
}

struct error *
lsi_addrange(struct lsi *lsi, struct lsi *lsi0)
{
	int i;

	struct le_arr *arr0 = lsi0->arr;
	for (i = 0; i < le_arr_len(arr0); i++) {
		struct lsi_le *le = lsi_le_copy(le_arr_get(arr0, i));
		struct error *err = lsi_add(lsi, le);
		if (err) {
			char *s = lsi_le_str(le);
			err = error_printf("%s not satisfiable", s);
			free(s);
			return err;
		}
	}
	return NULL;
}

static int
_satisfies(struct lsi *, struct lsi_le *);

struct error *
lsi_checksatisfiesrange(struct lsi *l, struct lsi *m)
{
	int i;

	struct le_arr *arr = m->arr;
	for (i = 0; i < le_arr_len(arr); i++) {
		struct lsi_le *le = le_arr_get(arr, i);
		if (!_satisfies(l, le)) {
			char *s = lsi_le_str(le);
			struct error *err = error_printf("%s not satisfied", s);
			free(s);
			return err;
		}
	}
	return NULL;
}

static int
_isorthogonal(struct lsi *, struct lsi_le *);

static int
_isfeasible(struct lsi *, struct lsi_le *);

static int
_satisfies(struct lsi *l, struct lsi_le *le)
{
	struct lsi_le *ng = lsi_le_negate(le);
	int ans = _isorthogonal(l, le) || !_isfeasible(l, ng);
	lsi_le_destroy(ng);
	return ans;
}

static int
_isorthogonal(struct lsi *l, struct lsi_le *le)
{
	int i;

	struct le_arr *arr = l->arr;
	for (i = 0; i < le_arr_len(arr); i++) {
		if (!_lsi_le_orthogonal(le, le_arr_get(arr, i))) {
			return 0;
		}
	}
	return 1;
}

static int
_isfeasible(struct lsi *l, struct lsi_le *le)
{
	struct lsi *copy = lsi_copy(l);
	struct error *err = lsi_add(copy, lsi_le_copy(le));
	lsi_destroy(copy);
	if (err) {
		assert(error_to_lsi_notfeasible(err));
		return 0;
	}
	return 1;
}
