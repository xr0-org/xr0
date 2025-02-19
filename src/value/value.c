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

#include "number_value.h"

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

struct number_value;

struct number_value *
number_value_min_create(void);

struct number_value *
number_value_max_create(void);

struct number_value *
number_value_constant_create(int constant);

static struct number *
number_singlerange_create(struct number_value *lw, struct number_value *up);

struct value *
value_ptr_rconst_create(void)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_PTR;
	v->ptr.isindefinite = true;
	v->ptr.n = number_singlerange_create(
		number_value_min_create(), number_value_max_create()
	);
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
	char *s = v->ptr.isindefinite ? number_str(v->ptr.n) : location_str(v->ptr.loc);
	strbuilder_printf(b, "ptr:%s", s);
	free(s);
}

struct value *
value_int_create(int val)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_INT;
	v->n = number_single_create(val);
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

struct number *
number_ne_create(int not_val);

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

struct number *
number_with_range_create(int lw, int excl_up);

struct value *
value_int_range_create(int lw, int excl_up)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_INT;
	v->n = number_with_range_create(lw, excl_up);
	return v;
}

static struct number_value *
range_limit_to_value(struct ast_expr *limit, struct state *s);

struct value *
value_int_rconst_create(struct ast_expr *range, struct state *s)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_INT;
	v->n = number_singlerange_create(
		range_limit_to_value(ast_expr_range_lw(range), s),
		range_limit_to_value(ast_expr_range_up(range), s)
	);
	return v;
}

static int
_constant(struct ast_expr *);

static struct number_value *
range_limit_to_value(struct ast_expr *limit, struct state *s)
{
	if (ast_expr_israngemax(limit)) {
		return number_value_max_create();
	} else if (ast_expr_israngemin(limit)) {
		return number_value_min_create();
	}
	return number_value_constant_create(_constant(limit));
}

static int
_constant(struct ast_expr *expr)
{
	return ast_expr_isnegative(expr)
		? -1 * _constant(ast_expr_negative_operand(expr))
		: ast_expr_as_constant(expr);
}

static int
number_range_lw(struct number *);

int
value_int_lw(struct value *v)
{
	assert(v->type == VALUE_RCONST || v->type == VALUE_INT);
	return number_range_lw(v->n);
}

static int
number_range_up(struct number *);

int
value_int_up(struct value *v)
{
	assert(v->type == VALUE_RCONST || v->type == VALUE_INT);
	return number_range_up(v->n);
}

static int
value_issinglerange(struct value *, struct state *);

int
value_as_int(struct value *v, struct state *s)
{
	assert(v->type == VALUE_INT && value_issinglerange(v, s));
	return value_int_lw(v);
}

struct number *
number_computed_create(struct ast_expr *);

struct value *
value_rconst_create(struct ast_expr *e)
{	
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_RCONST;
	v->n = number_computed_create(e);
	return v;
}

static struct number *
number_computed_bang(struct number *);

static struct value *
value_rconst_bang(struct value *orig)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_RCONST;
	v->n = number_computed_bang(orig->n);
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
		if (!value_issync(obj_value)) {
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

bool
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
		[VALUE_PTR] = "ptr",
		[VALUE_INT] = "int",
		[VALUE_LITERAL] = "literal",
		[VALUE_STRUCT] = "struct",
	};
	return dynamic_str(value_type_str[v->type]);
}

bool
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

bool
value_isconstant(struct value *v);

int
number_as_constant(struct number *n);

int
value_as_constant(struct value *v)
{
	assert(v->type == VALUE_INT);

	return number_as_constant(v->n);
}

bool
number_isconstant(struct number *n);

bool
value_isconstant(struct value *v)
{
	if (v->type != VALUE_INT) {
		return false;
	}
	return number_isconstant(v->n);
}

bool
number_issync(struct number *n);

bool
value_issync(struct value *v)
{
	if (v->type != VALUE_RCONST) {
		return false;
	}
	return number_issync(v->n);
}

struct ast_expr *
number_as_sync(struct number *n);

struct ast_expr *
value_as_rconst(struct value *v)
{
	assert(v->type == VALUE_RCONST);
	return number_as_sync(v->n);
}

struct ast_expr *
number_to_expr(struct number *n);

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
number_computed_equal(struct number *n1, struct number *n2);

static bool
samerconst(struct value *v, struct value *v0)
{
	return v->type == VALUE_RCONST
		&& v->type == v0->type
		&& number_computed_equal(v->n, v0->n);
}

static struct number_value *
value_lw(struct value *, struct state *);

static struct number_value *
value_up(struct value *, struct state *);

int
value_lt(struct value *lhs, struct value *rhs, struct state *s)
{
	assert(value_issinglerange(lhs, s) && value_issinglerange(rhs, s));

	struct number_value *a = value_lw(lhs, s),
			    *b = value_up(lhs, s),
			    *c = value_lw(rhs, s),
			    *d = value_up(rhs, s);

	if (number_value_le(b, c)) { /* b ≤ c ==> lhs < rhs */
		return 1;
	} 
	assert(
		number_value_ge(a, d) /* rhs < lhs */
		/* our assumption is that if there is overlap it is perfect */
		|| samerconst(lhs, rhs)
		|| (number_value_eq(a, c) && number_value_eq(b, d))
	);
	return 0;
}

int
number_value_as_constant(struct number_value *v);

int
value_eq(struct value *lhs, struct value *rhs, struct state *s)
{
	assert(value_issinglerange(lhs, s) && value_issinglerange(rhs, s));

	struct number_value *a = value_lw(lhs, s),
			    *b = value_up(lhs, s),
			    *c = value_lw(rhs, s),
			    *d = value_up(rhs, s);

	if (number_value_eq(a, c) && number_value_eq(b, d)) {
		if (!samerconst(lhs, rhs)) {
			int c_a = number_value_as_constant(a),
			    c_b = number_value_as_constant(b);
			assert(c_b-c_a == 1);
		}
		return 1;
	}

	return 0;
}


static void
value_splitto(struct value *, struct number *, struct map *, struct state *);

static struct number *
number_singlerange_create(struct number_value *lw, struct number_value *up);

struct error *
value_disentangle(struct value *x, struct value *y, struct state *s)
{
	assert(value_issinglerange(x, s) && value_issinglerange(y, s));

	struct number_value *a = value_lw(x, s),
			    *b = value_up(x, s),
			    *c = value_lw(y, s),
			    *d = value_up(y, s);

	/* 
	 * Our analysis begins with x in [a?b] and y in [c?d].
	 *
	 * 	<--------------->|——–––———————————–––|<------------>
	 * 	                 a                   b
	 *
	 * 	<--------------------->|––––––––––––––––|<--------->
	 * 	                       c                d
	 * 
	 */
	if (number_value_lt(c, a)) {
		return value_disentangle(y, x, s);
	}
	/* ⊢ a ≤ c */

	/* 
	 * WLOG, assume that a ≤ c. The relative locations of b, c, d are
	 * undecided, but we know that [a?b] is the range that begins first (or
	 * they begin at the same place):
	 *
	 * 	|——–––———————————–––|<----------------------------->
	 * 	a                   b
	 *
	 * 	<--------------------->|––––––––––––––––|<--------->
	 * 	                       c                d
	 */

	/* the rule is that if b ≤ c or a ≥ d we have no overlap. the previous
	 * step eliminated the possibility of a ≥ d */

	if (number_value_le(b, c)) {
		/* base case: no overlap */
		return NULL;
	}
	/* ⊢ b > c */

	/* 
	 * There is overlap:
	 *
	 * 	|——–––———————————–––|<----------------------->
	 * 	a                   b
	 *
	 * 	<--------------->|––––––––––––––––|<--------->
	 * 	                 c                d
	 */

	if (number_value_lt(a, c)) {
		/* split into two scenarios:
		 * 	- x in [a?c], y in [c?d] (x < y)
		 * 	- x in [c?b], y in [c?d] (same lower bound) */
		struct splitinstruct *splits = splitinstruct_create();
		struct map *x_lt_y = map_create(),
			   *a_eq_c = map_create();
		value_splitto(x, number_singlerange_create(a, c), x_lt_y, s);
		value_splitto(x, number_singlerange_create(c, b), a_eq_c, s);
		splitinstruct_append(splits, x_lt_y);
		splitinstruct_append(splits, a_eq_c);
		return error_verifierinstruct(verifierinstruct_split(splits));
	}
	/* ⊢ a == c */

	/* 
	 * Overlapping ranges that start at the same point:
	 *
	 * 	|——–––———————————–––|<------------------>
	 * 	a                   b
	 *
	 * 	|––––––––––––––––|<--------------------->
	 * 	c                d
	 */

	if (number_value_lt(d, b)) {
		return value_disentangle(y, x, s);
	}
	/* ⊢ b ≤ d */

	/* 
	 * WLOG we assume that the first range finishes before (or at the same
	 * point as) the second:
	 *
	 * 	|——–––————————––|<------>
	 * 	a               b
	 *
	 * 	|–––––––––––––––––––––––|
	 * 	c                       d
	 */

	if (number_value_lt(b, d)) {
		/* split into two scenarios:
		 * 	- x in [a?b], y in [b?d] (x < y)
		 * 	- x, y in [a?b] (perfect overlap) */
		/* ends in return */
		struct splitinstruct *splits = splitinstruct_create();
		struct map *x_lt_y = map_create(),
			   *b_eq_d = map_create();
		value_splitto(y, number_singlerange_create(b, d), x_lt_y, s);
		value_splitto(y, number_singlerange_create(a, b), b_eq_d, s);
		splitinstruct_append(splits, x_lt_y);
		splitinstruct_append(splits, b_eq_d);
		return error_verifierinstruct(verifierinstruct_split(splits));
	}
	/* ⊢ b == d */

	/* 
	 * Perfect overlap:
	 *
	 * 	|——–––————————––|
	 * 	a               b
	 *
	 * 	|–––––––––––––––|
	 * 	c               d
	 */

	if (samerconst(x, y)) {
		return NULL;
	}

	int c_a = number_value_as_constant(a),
	    c_b = number_value_as_constant(b);
	a_printf(
		c_b-c_a==1,
		"perfect overlap assumed to be at single points only for now\n"
	);

	return NULL;
}

static struct number_value *
number_lw(struct number *, struct state *);

static struct number_value *
number_up(struct number *, struct state *);

static struct number_value *
_value_bound(struct value *v, struct state *s, int islw);

static struct number_value *
value_lw(struct value *v, struct state *s)
{
	return _value_bound(v, s, 1);
}

static struct number_value *
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

static struct number_value *
value_up(struct value *v, struct state *s)
{
	return _value_bound(v, s, 0);
}

static int
number_issinglerange(struct number *, struct state *);

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

static void
number_splitto(struct number *, struct number *range, struct map *splits,
		struct state *);

static void
value_splitto(struct value *v, struct number *range, struct map *splits,
		struct state *s)
{
	switch (v->type) {
	case VALUE_INT:
		assert(
			value_issinglerange(v, s)
			&& number_value_eq(value_lw(v, s), number_lw(range, s))
			&& number_value_eq(value_up(v, s), number_up(range, s))
		);
		break;
	case VALUE_RCONST:
		number_splitto(v->n, range, splits, s);
		break;
	default:
		assert(false);
	}
}


static bool
number_assume(struct number *n, struct number *split);

bool
value_splitassume(struct value *v, struct number *split)
{
	switch (v->type) {
	case VALUE_INT:
		return number_assume(v->n, split);
	case VALUE_PTR:
		assert(v->ptr.isindefinite);
		return number_assume(v->ptr.n, split);
	default:
		assert(false);
	}
}

static int
int_or_rconst(struct value *);

static struct value *
value_tosinglerange(struct value *, struct state *);

char *
number_value_str(struct number_value *v);

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

	struct number_value *v_lw = value_lw(v, s),
			    *r0_lw = value_lw(r0, s0);
	if (!number_value_le(r0_lw, v_lw)) {
		return error_value_bounds(
			error_printf("must be ≥ %s", number_value_str(r0_lw))
		);
	}
	struct number_value *v_up = value_up(v, s),
			    *r0_up = value_up(r0, s0);
	if (!number_value_le(v_up, r0_up)) {
		return error_value_bounds(
			error_printf("must be < %s", number_value_str(r0_up))
		);
	}
	return NULL;
}

static int
int_or_rconst(struct value *v)
{
	return v->type == VALUE_INT || v->type == VALUE_RCONST;
}


static struct number *
number_tosinglerange(struct number *, struct state *);

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



struct number {
	enum number_type {
		NUMBER_RANGES,
		NUMBER_COMPUTED,
	} type;
	union {
		struct number_range_arr *ranges;
		struct ast_expr *computation;
	};
};

struct number *
number_ranges_create(struct number_range_arr *ranges)
{
	struct number *num = calloc(1, sizeof(struct number));
	num->type = NUMBER_RANGES;
	num->ranges = ranges;
	return num;
}

struct number_range_arr *
number_range_arr_single_create(int val);

struct number *
number_single_create(int val)
{
	return number_ranges_create(number_range_arr_single_create(val));
}

struct number_range_arr *
number_range_arr_single_create(int val)
{
	struct number_range_arr *arr = number_range_arr_create();
	number_range_arr_append(
		arr, 
		number_range_create(
			number_value_constant_create(val),
			number_value_constant_create(val+1)
		)
	);
	return arr;
}

struct number *
number_computed_create(struct ast_expr *e)
{
	struct number *num = calloc(1, sizeof(struct number));
	num->type = NUMBER_COMPUTED;
	num->computation = e;
	return num;
}

static struct number *
number_computed_bang(struct number *orig)
{
	struct number *num = calloc(1, sizeof(struct number));
	num->type = NUMBER_COMPUTED;
	num->computation = ast_expr_inverted_copy(orig->computation, true);
	return num;
}

static struct number_value *
_number_bound(struct number *n, int islw, struct state *s);

static struct number_value *
number_lw(struct number *n, struct state *s)
{
	return _number_bound(n, 1, s);
}

static struct number_value *
number_up(struct number *n, struct state *s)
{
	return _number_bound(n, 0, s);
}

static struct number_value *
_ranges_bound(struct number *, int islw);

static struct number_value *
_rconst_bound(struct ast_expr *, int islw, struct state *);

static struct number_value *
_number_bound(struct number *n, int islw, struct state *s)
{
	switch (n->type) {
	case NUMBER_RANGES:
		return _ranges_bound(n, islw);
	case NUMBER_COMPUTED:
		return _rconst_bound(n->computation, islw, s);
	default:
		assert(false);
	}
}

static struct number_value *
number_range_lower(struct number_range *);

static struct number_value *
number_range_upper(struct number_range *);

static struct number_value *
_ranges_bound(struct number *n, int islw)
{
	assert(n->type == NUMBER_RANGES);
	assert(number_range_arr_n(n->ranges) == 1);

	struct number_range *r = number_range_arr_range(n->ranges)[0];
	return islw ? number_range_lower(r) : number_range_upper(r);
}


static struct number_value *
_rconst_bound(struct ast_expr *e, int islw, struct state *s)
{
	struct value *rconst = tagval_value(
		tagval_res_as_tagval(ast_expr_rangeeval(e, s))
	);
	assert(rconst && rconst->type == VALUE_INT);
	return _number_bound(rconst->n, islw, s);
}

static int
_rconst_issinglerange(struct ast_expr *, struct state *);

static int
number_issinglerange(struct number *n, struct state *s)
{
	switch (n->type) {
	case NUMBER_RANGES:
		return number_range_arr_n(n->ranges) == 1;
	case NUMBER_COMPUTED:
		return _rconst_issinglerange(n->computation, s);
	default:
		assert(false);
	}
}

static int
_rconst_issinglerange(struct ast_expr *e, struct state *s)
{
	struct value *rconst = tagval_value(
		tagval_res_as_tagval(ast_expr_rangeeval(e, s))
	);
	assert(rconst && rconst->type == VALUE_INT);
	return number_issinglerange(rconst->n, s);
}


static void
number_splitto(struct number *n, struct number *range, struct map *splits,
		struct state *s)
{
	assert(
		n->type == NUMBER_COMPUTED
		&& number_issinglerange(n, s)
		&& number_value_le(number_lw(n, s), number_lw(range, s))
		&& number_value_le(number_up(range, s), number_up(n, s))
	);

	map_set(
		splits,
		dynamic_str(ast_expr_as_identifier(n->computation)),
		range
	);
}

static struct number *
_rconst_tosinglerange(char *, struct state *);

static struct number *
number_tosinglerange(struct number *n, struct state *s)
{
	switch (n->type) {
	case NUMBER_RANGES:
		assert(number_issinglerange(n, s));
		return n;
	case NUMBER_COMPUTED:
		return _rconst_tosinglerange(
			ast_expr_as_identifier(n->computation), s
		);
	default:
		assert(false);
	}
}

static struct number *
_rconst_tosinglerange(char *id, struct state *s)
{
	struct value *rconst = state_getrconst(s, id);
	assert(rconst && rconst->type == VALUE_INT);
	return number_tosinglerange(rconst->n, s);
}


struct number_value *
number_value_min_create(void);

struct number_value *
number_value_max_create(void);

struct number_range_arr *
number_range_arr_ne_create(int val)
{
	struct number_range_arr *arr = number_range_arr_create();
	number_range_arr_append(
		arr, 
		number_range_create( /* [MIN:val) */
			number_value_min_create(),
			number_value_constant_create(val)
		)
	);
	number_range_arr_append(
		arr, 
		number_range_create( /* [val+1:MAX) XXX */
			number_value_constant_create(val+1),
			number_value_max_create()
		)
	);
	return arr;
}

struct number *
number_ne_create(int val)
{
	return number_ranges_create(number_range_arr_ne_create(val));
}

struct number *
number_with_range_create(int lw, int excl_up)
{
	struct number_range_arr *arr = number_range_arr_create();
	number_range_arr_append(
		arr, 
		number_range_create( /* [lw:excl_up) */
			number_value_constant_create(lw),
			number_value_constant_create(excl_up)
		)
	);
	return number_ranges_create(arr);
}

static struct number *
number_singlerange_create(struct number_value *lw, struct number_value *up)
{
	struct number_range_arr *arr = number_range_arr_create();
	number_range_arr_append(
		arr, 
		number_range_create(lw, up)
	);
	return number_ranges_create(arr);
}

int
number_value_as_constant(struct number_value *v);

struct number_value *
number_range_lower(struct number_range *r);

static int
number_range_lw(struct number *n)
{
	assert(n->type == NUMBER_RANGES);
	assert(number_range_arr_n(n->ranges) == 1);

	struct number_range *r = number_range_arr_range(n->ranges)[0];
	return number_value_as_constant(number_range_lower(r));
}

static int
number_range_up(struct number *n)
{
	assert(n->type == NUMBER_RANGES);
	assert(number_range_arr_n(n->ranges) == 1);

	struct number_range *r = number_range_arr_range(n->ranges)[0];
	return number_value_as_constant(number_range_upper(r));
}

void
number_destroy(struct number *n)
{
	switch (n->type) {
	case NUMBER_RANGES:
		number_range_arr_destroy(n->ranges);
		break;
	case NUMBER_COMPUTED:
		ast_expr_destroy(n->computation);
		break;
	default:
		assert(false);
	}
}


char *
number_range_str(struct number_range *r);

char *
number_ranges_sprint(struct number *num)
{
	assert(num->type == NUMBER_RANGES);

	struct strbuilder *b = strbuilder_create();
	int n = number_range_arr_n(num->ranges);
	struct number_range **range = number_range_arr_range(num->ranges);
	strbuilder_putc(b, '{');
	for (int i = 0; i < n; i++) {
		char *r = number_range_str(range[i]);
		strbuilder_printf(b, "%s%s", r, (i+1 < n ? ", " : ""));
		free(r);
	}
	strbuilder_putc(b, '}');
	return strbuilder_build(b);
}

char *
number_str(struct number *num)
{
	switch (num->type) {
	case NUMBER_RANGES:
		return number_ranges_sprint(num);
	case NUMBER_COMPUTED:
		return ast_expr_str(num->computation);
	default:
		assert(false);
	}
}

bool
number_ranges_equal(struct number *n1, struct number *n2);

static int
number_computed_equal(struct number *n1, struct number *n2)
{
	if (n1->type != n2->type) {
		return 0;
	}
	switch (n1->type) {
	case NUMBER_RANGES:
		return 0;
	case NUMBER_COMPUTED:
		return ast_expr_equal(n1->computation, n2->computation);
	default:
		assert(false);
	}
}

bool
number_range_equal(struct number_range *, struct number_range *);

bool
number_ranges_equal(struct number *n1, struct number *n2)
{
	assert(n1->type == n2->type && n1->type == NUMBER_RANGES);

	int len = number_range_arr_n(n1->ranges);
	if (len != number_range_arr_n(n2->ranges)) {
		return false;
	}

	struct number_range **n1_r = number_range_arr_range(n1->ranges),
			    **n2_r = number_range_arr_range(n2->ranges);
	for (int i = 0; i < len; i++) {
		if (!number_range_equal(n1_r[i], n2_r[i])) {
			return false;
		}
	}

	return true;
}

static bool
number_range_arr_containsrangearr(struct number_range_arr *arr,
		struct number_range_arr *range);

static bool
number_assume(struct number *n, struct number *split)
{
	assert(n->type == NUMBER_RANGES && split->type == NUMBER_RANGES);

	if (!number_range_arr_containsrangearr(n->ranges, split->ranges)) {
		return false;
	}

	n->ranges = split->ranges;

	return true;
}

bool
number_range_issingle(struct number_range *r);

bool
number_isconstant(struct number *n)
{
	assert(n->type == NUMBER_RANGES);

	return number_range_arr_n(n->ranges) == 1 &&
		number_range_issingle(number_range_arr_range(n->ranges)[0]);
}

int
number_range_as_constant(struct number_range *r);

int
number_as_constant(struct number *n)
{
	assert(n->type == NUMBER_RANGES
			&& number_range_arr_n(n->ranges) == 1);

	return number_range_as_constant(number_range_arr_range(n->ranges)[0]);
}

bool
number_issync(struct number *n)
{
	return n->type == NUMBER_COMPUTED;
}

struct ast_expr *
number_as_sync(struct number *n)
{
	assert(n->type == NUMBER_COMPUTED);

	return n->computation;
}

struct ast_expr *
number_ranges_to_expr(struct number_range_arr *);

struct ast_expr *
number_to_expr(struct number *n)
{
	switch (n->type) {
	case NUMBER_RANGES:
		return number_ranges_to_expr(n->ranges);
	case NUMBER_COMPUTED:
		return ast_expr_copy(number_as_sync(n));
	default:
		assert(false);
	}
}

struct number_range_arr *
number_range_arr_copy(struct number_range_arr *old);

struct number *
number_copy(struct number *num)
{
	switch (num->type) {
	case NUMBER_RANGES:
		return number_ranges_create(number_range_arr_copy(num->ranges));
	case NUMBER_COMPUTED:
		return number_computed_create(ast_expr_copy(num->computation));
	default:
		assert(false);
	}
}

struct splitinstruct {
	int n;
	struct map **m;
};

struct splitinstruct *
splitinstruct_create(void)
{
	return calloc(1, sizeof(struct splitinstruct));
}

void 
splitinstruct_append(struct splitinstruct *s, struct map *m)
{
	s->m = realloc(s->m, sizeof(struct map *) * ++s->n);
	assert(s->m);
	s->m[s->n-1] = m;
}

int 
splitinstruct_n(struct splitinstruct *s)
{
	return s->n;
}

struct map **
splitinstruct_splits(struct splitinstruct *s)
{
	return s->m;
}


struct number_range_arr {
	int n;
	struct number_range **range;
};

struct number_range_arr *
number_range_arr_create(void)
{
	struct number_range_arr *arr = calloc(1, sizeof(struct number_range_arr));
	assert(arr);
	return arr;
}

void
number_range_arr_destroy(struct number_range_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		number_range_destroy(arr->range[i]);	
	}
	free(arr->range);
	free(arr);
}

int
number_range_arr_n(struct number_range_arr *arr)
{
	return arr->n;
}

struct number_range **
number_range_arr_range(struct number_range_arr *arr)
{
	return arr->range;
}

int
number_range_arr_append(struct number_range_arr *arr, struct number_range *r)
{
	arr->range = realloc(arr->range, sizeof(struct number_range *) * ++arr->n);
	assert(arr->range);
	int loc = arr->n-1;
	arr->range[loc] = r;
	return loc;
}

struct number_range *
number_range_copy(struct number_range *);

struct number_range_arr *
number_range_arr_copy(struct number_range_arr *old)
{
	struct number_range_arr *new = number_range_arr_create();
	for (int i = 0; i < old->n; i++) {
		number_range_arr_append(new, number_range_copy(old->range[i]));
	}
	return new;
}

struct ast_expr *
number_ranges_to_expr(struct number_range_arr *arr)
{
	assert(number_range_arr_n(arr) == 1);

	return ast_expr_constant_create(
		number_range_as_constant(arr->range[0])
	);
}

static bool
number_range_arr_containsrange(struct number_range_arr *, struct number_range *);

static bool
number_range_arr_containsrangearr(struct number_range_arr *arr,
		struct number_range_arr *range_arr)
{
	for (int i = 0; i < range_arr->n; i++) {
		if (!number_range_arr_containsrange(arr, range_arr->range[i])) {
			return false;
		}
	}
	return true;
}

static bool
number_range_contains_range(struct number_range *r, struct number_range *r2);

static bool
number_range_arr_containsrange(struct number_range_arr *arr,
		struct number_range *range)
{
	for (int i = 0; i < arr->n; i++) {
		/* XXX: currently will assert false if arr->range[i] contains
		 * the lower bound of range but not the entire range, so we're
		 * asserting out the possibility of partial inclusion */
		if (number_range_contains_range(arr->range[i], range)) {
			return true;
		}
	}
	return false;
}


struct number_range {
	struct number_value *lower, *upper;
};

struct number_range *
number_range_create(struct number_value *lw, struct number_value *up)
{
	struct number_range *r = malloc(sizeof(struct number_range));
	r->lower = lw;
	r->upper = up;
	return r;
}

void
number_range_destroy(struct number_range *r)
{
	number_value_destroy(r->lower);
	number_value_destroy(r->upper);
	free(r);
}

struct number_value *
number_range_lower(struct number_range *r)
{
	return r->lower;
}

struct number_value *
number_range_upper(struct number_range *r)
{
	return r->upper;
}

char *
number_range_str(struct number_range *r)
{
	struct strbuilder *b = strbuilder_create();
	if (number_range_issingle(r)) {
		strbuilder_printf(b, "%s", number_value_str(r->lower));
	} else {
		strbuilder_printf(
			b, "%s?%s",
			number_value_str_inrange(r->lower),
			number_value_str_inrange(r->upper)
		);
	}
	return strbuilder_build(b);
}

struct number_range *
number_range_copy(struct number_range *r)
{
	return number_range_create(
		number_value_copy(r->lower), number_value_copy(r->upper)
	);
}

static bool
number_range_contains_range(struct number_range *r, struct number_range *r2)
{
	if (number_value_le(r->lower, r2->lower)) {
		/* XXX: exclude partial inclusion cases */
		assert(r->upper);
		assert(r2->upper);
		assert(number_value_le(r2->upper, r->upper));
		/* ⊢ r->lower ≤ r2->lower && r2->upper ≤ r->upper */
		return true;
	}
	/* r->lower > r2->lower so r2 cannot in be in r */
	return false;
}

bool
number_values_aresingle(struct number_value *v1, struct number_value *v2);

bool
number_range_issingle(struct number_range *r)
{
	return number_values_aresingle(r->lower, r->upper);
}

bool
number_range_equal(struct number_range *r1, struct number_range *r2)
{
	return number_value_eq(r1->lower, r2->lower)
		&& number_value_eq(r1->upper, r2->upper);
}

int
number_value_as_constant(struct number_value *v);

int
number_range_as_constant(struct number_range *r)
{
	assert(number_range_issingle(r));

	return number_value_as_constant(r->lower);
}
