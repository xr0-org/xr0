#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>

#include "util.h"
#include "lsi.h"
#include "_limits.h"

#include "expr.h"
#include "expr_arr.h"
#include "le.h"
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
	for (i = 0; i < string_arr_len(elim); i++) {
		struct le_arr *temp = new;
		new = _eliminate(temp, string_arr_get(elim, i));
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
	for (i = 0; i < string_arr_len(vars); i++) {
		char *var = string_arr_get(vars, i);
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
		for (j = 0; j < string_arr_len(vararr); j++) {
			char *var = string_arr_get(vararr, j);
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

	for (i = 0; i < string_arr_len(arr); i++)
		if (strcmp(s, string_arr_get(arr, i)) == 0)
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
	for (i = 0; i < string_arr_len(vars); i++) {
		struct le_arr *temp = arr;
		arr = _eliminate(temp, string_arr_get(vars, i));
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

struct error *
lsi_checksatisfiesrange(struct lsi *l, struct lsi *m)
{
	int i;

	struct le_arr *arr = m->arr;
	for (i = 0; i < le_arr_len(arr); i++) {
		struct lsi_le *le = le_arr_get(arr, i);
		if (!lsi_satisfies(l, le)) {
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

int
lsi_satisfies(struct lsi *l, struct lsi_le *le)
{
	struct lsi_le *ng = lsi_le_negate(le);
	int ans = (_isorthogonal(l, le) && _isfeasible(l, le))
		|| !_isfeasible(l, ng);
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

static char *
_unusedvar(struct lsi *);

static struct lsi *
_augment_and_eliminate_rest(struct lsi *, char *var, struct lsi_expr *);

static struct lsi *
_eliminate_exceptvar(struct lsi *, char *);

static int
_lowerbound(struct lsi *, char *);

static int
_upperbound(struct lsi *, char *);

struct lsi_range *
lsi_range_eval(struct lsi *l, struct lsi_expr *e)
{
	if (_lsi_expr_isconst(e)) {
		int c = _lsi_expr_constterm(e);
		return lsi_range_create(c, c);
	}

	char *id = _unusedvar(l);

	struct lsi *elim = _augment_and_eliminate_rest(l, id, e);

	/* if we have eliminated all other variables and all redundant formulae
	 * then there should remain only two inequalities, bounding our variable
	 * from above and beneath. */
	struct le_arr *arr = elim->arr;
	assert(le_arr_len(arr) == 2);

	struct lsi_range *r =
		lsi_range_create(_lowerbound(elim, id), _upperbound(elim, id));

	lsi_destroy(elim);

	free(id);

	return r;
}

static char *
_unusedvar_inmap(struct map *);

static char *
_unusedvar(struct lsi *l)
{
	struct map *m = _varstomap(l->arr);
	char *s = _unusedvar_inmap(m);
	map_destroy(m);
	return s;
}

static char *
_unusedvar_inmap(struct map *m)
{
	int i;

	for (i = 0; i < INT_MAX; i++) {
		struct strbuilder *b = strbuilder_create();
		strbuilder_printf(b, "@%d", i);
		char *s = strbuilder_build(b);
		if (!map_get(m, s))
			return s;
		free(s);
	}

	assert(0);
}

static struct lsi *
_augment(struct lsi *old, char *var, struct lsi_expr *e);

static struct lsi *
_augment_and_eliminate_rest(struct lsi *old, char *var, struct lsi_expr *e)
{
	struct lsi *new = _augment(old, var, e);
	struct lsi *elim = _eliminate_exceptvar(new, var);
	lsi_destroy(new);
	return elim;
}

static void
_assign(struct lsi *, char *var, struct lsi_expr *);

static struct lsi *
_augment(struct lsi *old, char *var, struct lsi_expr *e)
{
	struct lsi *new = lsi_copy(old);
	_assign(new, var, e);
	return new;
}

static void
_assign(struct lsi *lsi, char *id, struct lsi_expr *e)
{
	struct lsi_expr *var = lsi_expr_var_create(dynamic_str(id));
	le_arr_append(
		lsi->arr,
		lsi_le_create(lsi_expr_copy(var), lsi_expr_copy(e))
	);
	le_arr_append(
		lsi->arr,
		lsi_le_create(lsi_expr_copy(e), lsi_expr_copy(var))
	);
	lsi_expr_destroy(var);
}


static struct lsi *
_eliminate_exceptvar(struct lsi *old, char *var)
{
	struct string_arr *arr = string_arr_create();
	string_arr_append(arr, dynamic_str(var));
	struct lsi *new = lsi_eliminate_except(old, arr);
	string_arr_destroy(arr);
	return new;
}

static int
_lowerbound(struct lsi *lsi, char *var)
{
	int i;

	struct le_arr *arr = lsi->arr;

	for (i = 0; i < le_arr_len(arr); i++) {
		struct lsi_le *le = le_arr_get(arr, i);
		/* negate because in standard form the constant is on the rhs */
		int c = -_lsi_le_getstdformconst(le);
		if (_lsi_le_isconstlowerbound(le, var, c))
			return c;
	}

	assert(0);
}

static int
_upperbound(struct lsi *lsi, char *var)
{
	int i;

	struct le_arr *arr = lsi->arr;

	for (i = 0; i < le_arr_len(arr); i++) {
		struct lsi_le *le = le_arr_get(arr, i);
		int c = _lsi_le_getstdformconst(le);
		if (_lsi_le_isconstupperbound(le, var, c))
			return c;
	}

	assert(0);
}
