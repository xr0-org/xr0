#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>

#include "ast.h"
#include "state.h"
#include "object.h"
#include "util.h"
#include "value.h"
#include "verifier.h"

#include "number.h"
#include "range.h"
#include "_limits.h"

struct value {
	enum value_type {
		VALUE_RCONST,
		VALUE_PTR,
		VALUE_INT,
		VALUE_LITERAL,
		VALUE_STRUCT,
	} type;
	union {
		struct {
			bool isindefinite;
			union {
				/* NULL is represented as NULL location */
				struct location *loc;

				/* particularly for "indefinite" case */
				struct number *n;
			};
		} ptr;

		/* TODO: int, char, etc */
		struct number *n;
		char *s;

		struct {
			struct ast_variable_arr *members;
			struct map *m; /* maps to objects */
		} _struct;
	};
}; 

static struct int_arr *
struct_deriveorder(struct value *v, struct circuitbreaker *cb, struct state *s);

struct int_arr *
value_deriveorder(struct value *v, struct circuitbreaker *cb, struct state *s)
{
	switch (v->type) {
	case VALUE_RCONST:
	case VALUE_INT:
	case VALUE_LITERAL:
		return int_arr_create();
	case VALUE_PTR:
		if (v->ptr.isindefinite) {
			return int_arr_create();
		}
		return location_deriveorder(v->ptr.loc, cb, s);
	case VALUE_STRUCT:
		return struct_deriveorder(v, cb, s);
	default:
		assert(false);
	}
}

static struct value *
struct_permuteheaplocs(struct value *, struct permutation *);

struct value *
value_permuteheaplocs(struct value *v, struct permutation *p)
{
	switch (v->type) {
	case VALUE_RCONST:
	case VALUE_INT:
	case VALUE_LITERAL:
		return value_copy(v);
	case VALUE_PTR:
		if (v->ptr.isindefinite) {
			return value_copy(v);
		}
		return value_ptr_create(location_permuteheap(v->ptr.loc, p));
	case VALUE_STRUCT:
		return struct_permuteheaplocs(v, p);
	default:
		assert(false);
	}
}

struct value *
value_ptr_create(struct location *loc)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_PTR;
	v->ptr.isindefinite = false;
	v->ptr.loc = loc;
	return v;
}

struct value *
value_ptr_rconst_create(void)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_PTR;
	v->ptr.isindefinite = true;
	v->ptr.n = number_range_create(range_entire_create());
	return v;
}

bool
ptr_referencesheap(struct value *v, struct state *s, struct circuitbreaker *cb)
{
	return !v->ptr.isindefinite && location_referencesheap(v->ptr.loc, s, cb);
}

struct number *
number_copy(struct number *num);

struct value *
value_ptr_copy(struct value *old)
{
	struct value *new = malloc(sizeof(struct value));
	assert(new);
	new->type = VALUE_PTR;
	new->ptr.isindefinite = old->ptr.isindefinite;
	if (old->ptr.isindefinite) {
		new->ptr.n = number_copy(old->ptr.n);
	} else {
		new->ptr.loc = location_copy(old->ptr.loc);
	}
	return new;
}

void
value_ptr_sprint(struct value *v, struct strbuilder *b)
{
	char *s = v->ptr.isindefinite ?
		number_str(v->ptr.n) : location_str(v->ptr.loc);
	strbuilder_printf(b, "ptr:%s", s);
	free(s);
}

struct value *
value_int_create(int val)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_INT;
	v->n = number_const_create(val);
	return v;
}

struct value *
value_number_create(struct number *n)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_INT;
	v->n = n;
	return v;
}

int
value_isint(struct value *v)
{
	return v->type == VALUE_INT;
}

struct value *
value_literal_create(char *lit)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_LITERAL;
	v->s = dynamic_str(lit);
	return v;
}

struct value *
value_int_ne_create(int not_val)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_INT;
	v->n = number_ne_create(not_val);
	return v;
}

static struct value *
value_rconst_bang(struct value *);

struct value *
value_bang(struct value *v)
{
	if (value_isconstant(v)) {
		return value_int_create(!value_as_constant(v));
	}
	switch (v->type) {
	case VALUE_RCONST:
		return value_rconst_bang(v);
	default:
		assert(false);
	}
}

struct value *
value_int_range_create(int lw, int up)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_INT;
	v->n = number_range_create(range_create(lw, up));
	return v;
}

struct value *
value_int_range_fromexpr(struct ast_expr *e, struct state *s)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_INT;
	v->n = number_range_create(range_fromexpr(e, s));
	return v;
}

long
value_int_lw(struct value *v, struct state *s)
{
	assert(v->type == VALUE_RCONST || v->type == VALUE_INT);
	return number_as_const(number_lw(v->n, s));
}

long
value_int_up(struct value *v, struct state *s)
{
	assert(v->type == VALUE_RCONST || v->type == VALUE_INT);
	return number_as_const(number_up(v->n, s));
}

static int
value_issinglerange(struct value *, struct state *);

int
value_as_int(struct value *v, struct state *s)
{
	assert(v->type == VALUE_INT && value_issinglerange(v, s));
	return value_int_lw(v, s);
}

struct number *
value_as_number(struct value *v)
{
	assert(v->type == VALUE_INT);
	return v->n;
}

struct value *
value_rconst_create(struct ast_expr *e)
{	
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_RCONST;
	v->n = number_expr_create(e);
	return v;
}

static struct value *
value_rconst_bang(struct value *orig)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_RCONST;
	v->n = number_expr_create(
		ast_expr_inverted_copy(number_as_expr(orig->n), true)
	);
	return v;
}

struct value *
value_rconst_copy(struct value *old)
{
	assert(old->type == VALUE_RCONST);

	struct value *new = malloc(sizeof(struct value));
	assert(new);
	new->type = VALUE_RCONST;
	new->n = number_copy(old->n);

	return new;
}

struct value *
value_int_copy(struct value *old)
{
	assert(old->type == VALUE_INT);

	struct value *new = malloc(sizeof(struct value));
	assert(new);
	new->type = VALUE_INT;
	new->n = number_copy(old->n);

	return new;
}

static struct map *
frommembers(struct ast_variable_arr *);

struct value *
value_struct_create(struct ast_type *t)
{
	struct ast_variable_arr *members = ast_variable_arr_copy(ast_type_struct_members(t));
	assert(members);

	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_STRUCT;
	v->_struct.members = members;
	v->_struct.m = frommembers(members);

	return v;
}

struct value *
value_struct_rconst_create(struct ast_type *t, struct state *s,
		char *key, bool persist)
{
	t = ast_type_struct_complete(t, state_getext(s));
	assert(ast_type_struct_members(t));

	struct value *v = value_struct_create(t);

	int n = ast_variable_arr_n(v->_struct.members);
	struct ast_variable **var = ast_variable_arr_v(v->_struct.members);
	for (int i = 0; i < n; i++) {
		char *field = ast_variable_name(var[i]);
		struct object *obj = map_get(v->_struct.m, field);
		struct strbuilder *b = strbuilder_create();
		strbuilder_printf(b, "%s.%s", key, field);
		struct ast_expr *range = ast_expr_range_create(
			strbuilder_build(b), /* key */
			ast_expr_rangemin_create(),
			ast_expr_rangemax_create()
		);
		object_assign(
			obj,
			state_rconst(
				s,
				ast_variable_type(var[i]),
				range,
				dynamic_str(ast_expr_range_key(range)),
				persist
			)
		);
	}

	return v;
}

struct value *
value_struct_rconstnokey_create(struct ast_type *t, struct state *s, bool persist)
{
	t = ast_type_struct_complete(t, state_getext(s));
	assert(ast_type_struct_members(t));

	struct value *v = value_struct_create(t);

	int n = ast_variable_arr_n(v->_struct.members);
	struct ast_variable **var = ast_variable_arr_v(v->_struct.members);
	for (int i = 0; i < n; i++) {
		char *field = ast_variable_name(var[i]);
		struct object *obj = map_get(v->_struct.m, field);
		struct ast_expr *range = ast_expr_range_create(
			dynamic_str("no key"),
			ast_expr_rangemin_create(),
			ast_expr_rangemax_create()
		);
		object_assign(
			obj,
			state_rconstnokey(
				s,
				ast_variable_type(var[i]),
				range,
				persist
			)
		);
	}

	return v;
}

struct value *
value_pf_augment(struct value *old, struct ast_expr *root)
{
	assert(value_isstruct(old));

	struct value *v = value_copy(old);

	int n = ast_variable_arr_n(v->_struct.members);
	struct ast_variable **var = ast_variable_arr_v(v->_struct.members);
	for (int i = 0; i < n; i++) {
		char *field = ast_variable_name(var[i]);
		struct object *obj = map_get(v->_struct.m, field);
		struct value *obj_value = object_as_value(obj);
		if (!obj_value) {
			continue;
		}
		if (!value_isrconst(obj_value)) {
			continue;
		}
		object_assign(
			obj,
			value_rconst_create(
				ast_expr_member_create(
					ast_expr_copy(root), dynamic_str(field)
				)
			)
		);
	}
	return v;
}

int
value_isstruct(struct value *v)
{
	return v->type == VALUE_STRUCT;
}

static struct map *
frommembers(struct ast_variable_arr *members)
{
	struct map *m = map_create();

	int n = ast_variable_arr_n(members);
	struct ast_variable **v = ast_variable_arr_v(members);
	for (int i = 0; i < n; i++) {
		map_set(
			m, dynamic_str(ast_variable_name(v[i])),
			object_value_create(
				ast_expr_constant_create(0), NULL
			)
		);
	}

	return m;
}

static void
destroymembers(struct map *m)
{
	for (int i = 0; i < m->n; i++) {
		object_destroy((struct object *) m->entry[i].value);
	}
}

static struct map *
copymembers(struct map *);

struct value *
value_struct_copy(struct value *old)
{
	struct value *new = malloc(sizeof(struct value));
	assert(new);
	new->type = VALUE_STRUCT;
	new->_struct.members = ast_variable_arr_copy(old->_struct.members);
	new->_struct.m = copymembers(old->_struct.m);
	return new;
}

static struct map *
copymembers(struct map *old)
{
	struct map *new = map_create();
	for (int i = 0; i < old->n; i++) {
		struct entry e = old->entry[i];
		map_set(new, dynamic_str(e.key), object_copy((struct object *) e.value));
	}
	return new;
}


static struct map *
permutemembers(struct map *old, struct permutation *);

static struct value *
struct_permuteheaplocs(struct value *old, struct permutation *p)
{
	struct value *new = malloc(sizeof(struct value));
	assert(new);
	new->type = VALUE_STRUCT;
	new->_struct.members = ast_variable_arr_copy(old->_struct.members);
	new->_struct.m = permutemembers(old->_struct.m, p);
	return new;
}

static struct map *
permutemembers(struct map *old, struct permutation *p)
{
	struct map *new = map_create();
	for (int i = 0; i < old->n; i++) {
		struct entry e = old->entry[i];
		map_set(
			new, dynamic_str(e.key),
			object_permuteheaplocs((struct object *) e.value, p)
		);
	}
	return new;
}

static struct int_arr *
struct_deriveorder(struct value *v, struct circuitbreaker *cb, struct state *s)
{
	struct int_arr *arr = int_arr_create();
	struct map *m = v->_struct.m;
	for (int i = 0; i < m->n; i++) {
		int_arr_appendrange(
			arr,
			object_deriveorder(
				(struct object *) m->entry[i].value, cb, s
			)
		);
	}
	return arr;
}

static struct map *
abstractcopymembers(struct map *old, struct state *s);

struct value *
value_struct_abstractcopy(struct value *old, struct state *s)
{
	struct value *new = malloc(sizeof(struct value));
	assert(new);
	new->type = VALUE_STRUCT;
	new->_struct.members = ast_variable_arr_copy(old->_struct.members);
	new->_struct.m = abstractcopymembers(old->_struct.m, s);
	return new;
}

static struct map *
abstractcopymembers(struct map *old, struct state *s)
{
	struct map *new = map_create();
	for (int i = 0; i < old->n; i++) {
		struct entry e = old->entry[i];
		map_set(
			new, dynamic_str(e.key),
			object_abstractcopy((struct object *) e.value, s)
		);
	}
	return new;
}

struct ast_type *
value_struct_membertype(struct value *v, char *member)
{
	struct ast_variable_arr *members = v->_struct.members;

	int n = ast_variable_arr_n(members);
	struct ast_variable **var = ast_variable_arr_v(members);
	for (int i = 0; i < n; i++) {
		if (strcmp(member, ast_variable_name(var[i])) == 0) {
			return ast_variable_type(var[i]);
		}
	}

	return NULL;
}

struct object *
value_struct_member(struct value *v, char *member)
{
	return map_get(v->_struct.m, member);
}

struct error *
value_struct_specval_verify(struct value *param, struct value *arg,
		struct state *spec, struct state *caller)
{
	assert(value_isstruct(param) && value_isstruct(arg));
	struct ast_variable_arr *param_members = param->_struct.members,
				*arg_members = arg->_struct.members;

	int n = ast_variable_arr_n(param_members);
	assert(ast_variable_arr_n(arg_members) == n);
	struct ast_variable **param_var = ast_variable_arr_v(param_members);
	for (int i = 0; i < n; i++) {
		char *field = ast_variable_name(param_var[i]);
		struct error *err = state_constraintverify_structmember(
			spec, caller, param, arg, field
		);
		if (err) {
			a_printf(
				false,
				"needs test and custom error message: %s\n",
				error_str(err)
			);
		}
	}

	return NULL;

}


bool
struct_referencesheap(struct value *v, struct state *s, struct circuitbreaker *cb)
{
	struct map *m = v->_struct.m;
	for (int i = 0; i < m->n; i++) {
		struct value *val = object_as_value((struct object *) m->entry[i].value);
		if (val && value_referencesheap(val, s, cb)) {
			return true;
		}
	}
	return false;
}

void
value_struct_sprint(struct value *v, struct strbuilder *b)
{
	strbuilder_printf(b, "struct:{");

	struct ast_variable_arr *members = v->_struct.members;

	int n = ast_variable_arr_n(members);
	struct ast_variable **var = ast_variable_arr_v(members);
	for (int i = 0; i < n; i++) {
		char *f = ast_variable_name(var[i]);
		struct object *obj = map_get(v->_struct.m, f);
		char *val_str = object_hasvalue(obj)
			? value_str(object_as_value(obj))
			: dynamic_str("");
		strbuilder_printf(b, ".%s = <%s>%s", f, val_str,
			i+1<n ? ", " : "");
		free(val_str);
	}

	strbuilder_printf(b, "}");
}

void
value_int_sprint(struct value *v, struct strbuilder *b)
{
	strbuilder_printf(b, "int:%s", number_str(v->n));
}

void
value_rconst_sprint(struct value *v, struct strbuilder *b)
{
	strbuilder_printf(b, "rconst:%s", number_str(v->n));
}

struct value *
value_copy(struct value *v)
{
	assert(v);
	switch (v->type) {
	case VALUE_RCONST:
		return value_rconst_copy(v);
	case VALUE_PTR:
		return value_ptr_copy(v);
	case VALUE_INT:
		return value_int_copy(v);
	case VALUE_LITERAL:
		return value_literal_create(v->s);
	case VALUE_STRUCT:
		return value_struct_copy(v);
	default:
		assert(false);
	}
}

static bool
referencesheap_withcb(struct value *, struct state *);


struct value *
value_abstractcopy(struct value *v, struct state *s)
{
	if (!referencesheap_withcb(v, s)) {
		return NULL;
	}
	switch (v->type) {
	case VALUE_PTR:
		return value_copy(v);
	case VALUE_STRUCT:
		return value_struct_abstractcopy(v, s);
	default:
		assert(false);
	}
}

static bool
referencesheap_withcb(struct value *v, struct state *s)
{
	struct circuitbreaker *cb = circuitbreaker_create();
	bool ans = value_referencesheap(v, s, cb);
	circuitbreaker_destroy(cb);
	return ans;
}

void
value_destroy(struct value *v)
{
	switch (v->type) {
	case VALUE_RCONST:
		number_destroy(v->n);
		break;
	case VALUE_PTR:
		if (v->ptr.isindefinite) {
			number_destroy(v->ptr.n);
		} else {
			if (v->ptr.loc) {
				location_destroy(v->ptr.loc);
			}
		}
		break;
	case VALUE_INT:
		number_destroy(v->n);
		break;
	case VALUE_LITERAL:
		free(v->s);
		break;
	case VALUE_STRUCT:
		ast_variable_arr_destroy(v->_struct.members);
		destroymembers(v->_struct.m);
		map_destroy(v->_struct.m);
		break;
	default:
		assert(false);
	}
	free(v);
}

char *
value_str(struct value *v)
{
	struct strbuilder *b = strbuilder_create();
	switch (v->type) {
	case VALUE_RCONST:
		value_rconst_sprint(v, b);
		break;
	case VALUE_PTR:
		value_ptr_sprint(v, b);
		break;
	case VALUE_INT:
		value_int_sprint(v, b);
		break;
	case VALUE_LITERAL:
		strbuilder_printf(b, "\"%s\"", v->s);
		break;
	case VALUE_STRUCT:
		value_struct_sprint(v, b);
		break;
	default:
		assert(false);
	}
	return strbuilder_build(b);
}

char *
value_type_str(struct value *v)
{
	char *value_type_str[] = {
		[VALUE_RCONST] = "rconst",
		[VALUE_PTR] 	= "ptr",
		[VALUE_INT]	= "int",
		[VALUE_LITERAL] = "literal",
		[VALUE_STRUCT]	= "struct",
	};
	return dynamic_str(value_type_str[v->type]);
}

int
value_islocation(struct value *v)
{
	assert(v);
	return v->type == VALUE_PTR && !v->ptr.isindefinite;
}

struct location *
value_as_location(struct value *v)
{
	assert(value_islocation(v));
	return v->ptr.loc;
}

bool
value_referencesheap(struct value *v, struct state *s, struct circuitbreaker *cb)
{
	switch (v->type) {
	case VALUE_PTR:
		return ptr_referencesheap(v, s, cb);
	case VALUE_STRUCT:
		return struct_referencesheap(v, s, cb);
	default:
		return false;
	}
}

int
value_isconstant(struct value *v)
{
	return v->type == VALUE_INT && number_isconst(v->n);
}

static int
_isint(long l);

int
value_as_constant(struct value *v)
{
	assert(value_isconstant(v));

	long l = number_as_const(v->n);
	assert(_isint(l));
	return l;
}

static int
_isint(long l) { return C89_INT_MIN <= l && l <= C89_INT_MAX; }

int
value_isrconst(struct value *v)
{
	if (v->type != VALUE_RCONST) {
		return 0;
	}
	return number_isexpr(v->n);
}

struct ast_expr *
value_as_rconst(struct value *v)
{
	assert(v->type == VALUE_RCONST);
	return number_as_expr(v->n);
}

struct ast_expr *
value_to_expr(struct value *v)
{
	switch (v->type) {
	case VALUE_PTR:
		return ast_expr_identifier_create(value_str(v));
	case VALUE_LITERAL:
		return ast_expr_copy(value_as_literal(v));
	case VALUE_RCONST:
		return ast_expr_copy(value_as_rconst(v));
	case VALUE_INT:
		return number_to_expr(v->n);
	default:
		assert(false);
	}
}

bool
value_isliteral(struct value *v)
{
	if (v->type != VALUE_LITERAL) {
		return false;
	}
	return true;
}

struct ast_expr *
value_as_literal(struct value *v)
{
	assert(v->type == VALUE_LITERAL);
	return ast_expr_literal_create(v->s);
}

DEFINE_RESULT_TYPE(struct value *, value, value_destroy, value_res, false)

struct value_arr {
	int n;
	struct value **v;
};

struct value_arr *
value_arr_create(void)
{
	return calloc(1, sizeof(struct value_arr));
}

void
value_arr_destroy(struct value_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		value_destroy(arr->v[i]);
	}
	free(arr);
}

void
value_arr_append(struct value_arr *arr, struct value *v)
{
	arr->v = realloc(arr->v, sizeof(struct value *) * ++arr->n);
	arr->v[arr->n-1] = v;
}

int
value_arr_len(struct value_arr *arr)
{
	return arr->n;
}

struct value **
value_arr_v(struct value_arr *arr)
{
	return arr->v;
}

DEFINE_RESULT_TYPE(struct value_arr *, arr, value_arr_destroy, value_arr_res, false)


static bool
struct_references(struct value *v, struct location *loc, struct state *s,
		struct circuitbreaker *);

bool
value_references(struct value *v, struct location *loc, struct state *s,
		struct circuitbreaker *cb)
{
	switch (v->type) {
	case VALUE_PTR:
		return !v->ptr.isindefinite && location_references(v->ptr.loc, loc, s, cb);
	case VALUE_STRUCT:
		return struct_references(v, loc, s, cb);
	default:
		/* other kinds of values cannot reference */
		return false;
	}
}

static bool
struct_references(struct value *v, struct location *loc, struct state *s,
		struct circuitbreaker *cb)
{
	struct map *m = v->_struct.m;
	for (int i = 0; i < m->n; i++) {
		struct value *val = object_as_value(
			(struct object *) m->entry[i].value
		);
		if (val && value_references(val, loc, s, cb)) {
			return true;
		}
	}
	return false;
}

bool
values_comparable(struct value *v1, struct value *v2)
{
	return v1->type == v2->type;
}

static void
nulldestruct(int x) { /* do nothing */ }

DEFINE_RESULT_TYPE(bool, bool, nulldestruct, bool_res, true)

static bool
samerconst(struct value *, struct value *);

static int
_number_expr_equal(struct number *n, struct number *n0);

static bool
samerconst(struct value *v, struct value *v0)
{
	return v->type == VALUE_RCONST
		&& v->type == v0->type
		&& _number_expr_equal(v->n, v0->n);
}

static int
_number_expr_equal(struct number *n, struct number *n0)
{
	return number_isexpr(n) && number_isexpr(n0)
		&& ast_expr_equal(number_as_expr(n), number_as_expr(n0));
}

static struct number *
value_lw(struct value *, struct state *);

static struct number *
value_up(struct value *, struct state *);

int
value_lt(struct value *lhs, struct value *rhs, struct state *s)
{
	assert(value_issinglerange(lhs, s) && value_issinglerange(rhs, s));

	struct number *a = value_lw(lhs, s),
		      *b = value_up(lhs, s),
		      *c = value_lw(rhs, s),
		      *d = value_up(rhs, s);

	if (number_le(b, c, s)) { /* b ≤ c ==> lhs < rhs */
		return 1;
	} 
	assert(
		number_ge(a, d, s) /* rhs < lhs */
		/* our assumption is that if there is overlap it is perfect */
		|| samerconst(lhs, rhs)
		|| (number_eq(a, c, s) && number_eq(b, d, s))
	);
	return 0;
}

int
value_eq(struct value *lhs, struct value *rhs, struct state *s)
{
	assert(value_issinglerange(lhs, s) && value_issinglerange(rhs, s));

	struct number *a = value_lw(lhs, s),
		      *b = value_up(lhs, s),
		      *c = value_lw(rhs, s),
		      *d = value_up(rhs, s);

	if (number_eq(a, c, s) && number_eq(b, d, s)) {
		if (!samerconst(lhs, rhs)) {
			long c_a = number_as_const(a),
			     c_b = number_as_const(b);
			assert(c_b-c_a == 1);
		}
		return 1;
	}

	return 0;
}


static int
int_or_rconst(struct value *);

struct error *
value_disentangle(struct value *x, struct value *y, struct state *s)
{
	assert(int_or_rconst(x) && int_or_rconst(y));
	return number_disentangle(x->n, y->n, s);
}

static struct number *
_value_bound(struct value *v, struct state *s, int islw);

static struct number *
value_lw(struct value *v, struct state *s)
{
	return _value_bound(v, s, 1);
}

static struct number *
_value_bound(struct value *v, struct state *s, int islw)
{
	switch (v->type) {
	case VALUE_INT:
	case VALUE_RCONST:
		return islw ? number_lw(v->n, s) : number_up(v->n, s);
	default:
		assert(false);
	}
}

static struct number *
value_up(struct value *v, struct state *s)
{
	return _value_bound(v, s, 0);
}

static int
value_issinglerange(struct value *v, struct state *s)
{
	switch (v->type) {
	case VALUE_INT:
	case VALUE_RCONST:
		return number_issinglerange(v->n, s);
	default:
		assert(false);
	}

}

int
value_splitassume(struct value *v, struct number *split, struct state *s)
{
	switch (v->type) {
	case VALUE_INT:
		return number_assume(v->n, split, s);
	case VALUE_PTR:
		assert(v->ptr.isindefinite);
		return number_assume(v->ptr.n, split, s);
	default:
		assert(false);
	}
}

static int
int_or_rconst(struct value *);

static struct value *
value_tosinglerange(struct value *, struct state *);

struct error *
value_confirmsubset(struct value *v, struct value *v0, struct state *s,
		struct state *s0)
{
	a_printf(
		int_or_rconst(v) && int_or_rconst(v0),
		"can only compare subset for int or rconst types\n"
	);
	assert(value_issinglerange(v, s));
	struct value *r0 = value_tosinglerange(v0, s0);

	struct number *v_lw = value_lw(v, s),
		      *r0_lw = value_lw(r0, s0);
	if (!number_le(r0_lw, v_lw, s)) {
		return error_value_bounds(
			error_printf("must be ≥ %s", number_short_str(r0_lw))
		);
	}
	struct number *v_up = value_up(v, s),
		      *r0_up = value_up(r0, s0);
	if (!number_le(v_up, r0_up, s)) {
		return error_value_bounds(
			error_printf("must be < %s", number_short_str(r0_up))
		);
	}
	return NULL;
}

static int
int_or_rconst(struct value *v)
{
	return v->type == VALUE_INT || v->type == VALUE_RCONST;
}


static struct value *
value_tosinglerange(struct value *old, struct state *s)
{
	switch (old->type) {
	case VALUE_INT:
	case VALUE_RCONST:
		break;
	default:
		assert(false);
	}

	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_INT;
	v->n = number_tosinglerange(old->n, s);
	return v;
}
