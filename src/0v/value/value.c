#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "state.h"
#include "object.h"
#include "util.h"
#include "value.h"

struct value {
	enum value_type type;
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

struct number *
number_indefinite_create();

struct value *
value_ptr_indefinite_create()
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_PTR;
	v->ptr.isindefinite = true;
	v->ptr.n = number_indefinite_create();
	return v;
}

static bool
ptr_referencesheap(struct value *v, struct state *s)
{
	return !v->ptr.isindefinite && location_referencesheap(v->ptr.loc, s);
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

struct number *
number_indefinite_create();

struct value *
value_int_indefinite_create()
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_INT;
	v->n = number_indefinite_create();
	return v;
}

int
number_range_lw(struct number *);

int
value_int_lw(struct value *v)
{
	return number_range_lw(v->n);
}

int
number_range_up(struct number *);


int
value_int_up(struct value *v)
{
	return number_range_up(v->n);
}


struct number *
number_computed_create(struct ast_expr *);

struct value *
value_sync_create(struct ast_expr *e)
{	
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_SYNC;
	v->n = number_computed_create(e);
	return v;
}

struct value *
value_sync_copy(struct value *old)
{
	assert(old->type == VALUE_SYNC);

	struct value *new = malloc(sizeof(struct value));
	assert(new);
	new->type = VALUE_SYNC;
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
value_struct_indefinite_create(struct ast_type *t, struct state *s,
		char *comment, bool persist)
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
		strbuilder_printf(b, "%s.%s", comment, field);
		object_assign(
			obj,
			state_vconst(
				s,
				ast_variable_type(var[i]),
				strbuilder_build(b), /* comment */
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
			value_sync_create(
				ast_expr_member_create(
					ast_expr_copy(root), dynamic_str(field)
				)
			)
		);
	}
	/*printf("root: %s\n", ast_expr_str(root));*/
	/*printf("old: %s\n", value_str(old));*/
	/*printf("new: %s\n", value_str(v));*/

	/*assert(false);*/
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

static bool
struct_referencesheap(struct value *v, struct state *s)
{
	struct map *m = v->_struct.m;
	for (int i = 0; i < m->n; i++) {
		struct value *val = object_as_value((struct object *) m->entry[i].value);
		if (val && value_referencesheap(val, s)) {
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
		struct value *val = object_as_value(map_get(v->_struct.m, f));
		char *val_str = val ? value_str(val) : dynamic_str("");
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
value_sync_sprint(struct value *v, struct strbuilder *b)
{
	strbuilder_printf(b, "comp:%s", number_str(v->n));
}

struct value *
value_copy(struct value *v)
{
	switch (v->type) {
	case VALUE_SYNC:
		return value_sync_copy(v);
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

struct value *
value_abstractcopy(struct value *v, struct state *s)
{
	printf("value: %s\n", value_str(v));
	if (!value_referencesheap(v, s)) {
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

void
value_destroy(struct value *v)
{
	switch (v->type) {
	case VALUE_SYNC:
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
	case VALUE_SYNC:
		value_sync_sprint(v, b);
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
value_referencesheap(struct value *v, struct state *s)
{
	switch (v->type) {
	case VALUE_PTR:
		return ptr_referencesheap(v, s);
	case VALUE_STRUCT:
		return struct_referencesheap(v, s);
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
	if (v->type != VALUE_SYNC) {
		return false;
	}
	return number_issync(v->n);
}

struct ast_expr *
number_as_sync(struct number *n);

struct ast_expr *
value_as_sync(struct value *v)
{
	assert(v->type == VALUE_SYNC);
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
	case VALUE_SYNC:
		return ast_expr_copy(value_as_sync(v));
	case VALUE_INT:
		return number_to_expr(v->n);
	default:
		/*printf("v: %s\n", value_str(v));*/
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

enum value_type
value_type(struct value *v)
{
	return v->type;
}

static bool
struct_references(struct value *v, struct location *loc, struct state *s);

bool
value_references(struct value *v, struct location *loc, struct state *s)
{
	switch (v->type) {
	case VALUE_PTR:
		return !v->ptr.isindefinite && location_references(v->ptr.loc, loc, s);
	case VALUE_STRUCT:
		return struct_references(v, loc, s);
	default:
		/* other kinds of values cannot reference */
		return false;
	}
}

static bool
struct_references(struct value *v, struct location *loc, struct state *s)
{
	struct map *m = v->_struct.m;
	for (int i = 0; i < m->n; i++) {
		struct value *val = object_as_value(
			(struct object *) m->entry[i].value
		);
		if (val && value_references(val, loc, s)) {
			return true;
		}
	}
	return false;
}

bool
number_equal(struct number *n1, struct number *n2);


bool
values_comparable(struct value *v1, struct value *v2)
{
	return v1->type == v2->type;
}

bool
value_equal(struct value *v1, struct value *v2)
{
	assert(v1->type == v2->type);

	switch (v1->type) {
	case VALUE_LITERAL:
		return strcmp(v1->s, v2->s) == 0;
	case VALUE_INT:
	case VALUE_SYNC:
		return number_equal(v1->n, v2->n);
	default:
		assert(false);
	}
}

static bool
number_assume(struct number *, bool value);

bool
value_assume(struct value *v, bool value)
{
	switch (v->type) {
	case VALUE_INT:
		return number_assume(v->n, value);
	case VALUE_PTR:
		assert(v->ptr.isindefinite);
		return number_assume(v->ptr.n, value);
	default:
		assert(false);
	}
}

struct number {
	enum number_type type;
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

struct number_value *
number_value_constant_create(int constant);

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

struct number_value *
number_value_min_create();

struct number_value *
number_value_max_create();

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

struct number *
number_indefinite_create()
{
	struct number_range_arr *arr = number_range_arr_create();
	number_range_arr_append(
		arr, 
		number_range_create(
			number_value_min_create(),
			number_value_max_create()
		)
	);
	return number_ranges_create(arr);
}

int
number_value_as_constant(struct number_value *v);

struct number_value *
number_range_lower(struct number_range *r);

int
number_range_lw(struct number *n)
{
	assert(number_range_arr_n(n->ranges) == 1);

	struct number_range *r = number_range_arr_range(n->ranges)[0];
	return number_value_as_constant(number_range_lower(r));
}

struct number_value *
number_range_upper(struct number_range *r);

int
number_range_up(struct number *n)
{
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

bool
number_equal(struct number *n1, struct number *n2)
{
	assert(n1->type == n2->type);

	switch (n1->type) {
	case NUMBER_RANGES:
		return number_ranges_equal(n1, n2);
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
number_range_arr_canbe(struct number_range_arr *arr, bool value);

static struct number_range_arr *
number_range_assumed_value(bool value);

static bool
number_assume(struct number *n, bool value)
{
	assert(n->type == NUMBER_RANGES);

	if (!number_range_arr_canbe(n->ranges, value)) {
		return false;
	}

	n->ranges = number_range_assumed_value(value);

	return true;
}

static struct number_range_arr *
number_range_assumed_value(bool value)
{
	if (value) {
		return number_range_arr_ne_create(0);
	} else {
		return number_range_arr_single_create(0);
	}
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


struct number_range_arr {
	int n;
	struct number_range **range;
};

struct number_range_arr *
number_range_arr_create()
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
number_range_canbe(struct number_range *, bool value);

static bool
number_range_arr_canbe(struct number_range_arr *arr, bool value)
{
	for (int i = 0; i < arr->n; i++) {
		if (number_range_canbe(arr->range[i], value)) {
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
number_value_str(struct number_value *v);

char *
number_range_str(struct number_range *r)
{
	struct strbuilder *b = strbuilder_create();
	if (number_range_issingle(r)) {
		strbuilder_printf(b, "%s", number_value_str(r->lower));
	} else {
		strbuilder_printf(b, "%s:%s", number_value_str(r->lower),
				number_value_str(r->upper));
	}
	return strbuilder_build(b);
}

struct number_value *
number_value_copy(struct number_value *v);

struct number_range *
number_range_copy(struct number_range *r)
{
	return number_range_create(
		number_value_copy(r->lower), number_value_copy(r->upper)
	);
}

/* number_value_le: v ≤ constant */
static bool
number_value_le_constant(struct number_value *v, int constant);

static bool
constant_le_number_value(int constant, struct number_value *v);

bool
number_value_equal(struct number_value *v1, struct number_value *v2);

static bool
number_range_canbe(struct number_range *r, bool value)
{
	if (value) {
		if (number_value_equal(r->lower, r->upper)) {
			/* empty range */
			return false;
		}
		/* r->lower ≤ -1 || 1 ≤ r->lower */
		return number_value_le_constant(r->lower, -1) ||
			constant_le_number_value(1, r->lower);
	} else {
		/* check if r contains zero */
		/* r->lower ≤ 0 && 0 < r->upper (non-inclusive upper)  */
		return number_value_le_constant(r->lower, 0)
			&& constant_le_number_value(1, r->upper);
	}
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
	return number_value_equal(r1->lower, r2->lower)
		&& number_value_equal(r1->upper, r2->upper);
}

int
number_value_as_constant(struct number_value *v);

int
number_range_as_constant(struct number_range *r)
{
	assert(number_range_issingle(r));

	return number_value_as_constant(r->lower);
}

struct number_value {
	enum number_value_type type;
	union {
		int constant;
		bool max;
	};
};

struct number_value *
number_value_constant_create(int constant)
{
	struct number_value *v = malloc(sizeof(struct number_value));
	assert(v);
	v->type = NUMBER_VALUE_CONSTANT;
	v->constant = constant;
	return v;
}

struct number_value *
number_value_limit_create(bool max)
{
	struct number_value *v = malloc(sizeof(struct number_value));
	assert(v);
	v->type = NUMBER_VALUE_LIMIT;
	v->max = max;
	return v;
}

struct number_value *
number_value_min_create()
{
	return number_value_limit_create(false);
}

struct number_value *
number_value_max_create()
{
	return number_value_limit_create(true);
}

void
number_value_destroy(struct number_value *v)
{
	free(v);
}

char *
number_value_str(struct number_value *v)
{
	struct strbuilder *b = strbuilder_create();
	switch (v->type) {
	case NUMBER_VALUE_CONSTANT:
		strbuilder_printf(b, "%d", v->constant);
		break;
	case NUMBER_VALUE_LIMIT:
		strbuilder_printf(b, "%s", v->max ? "MAX" : "MIN");
		break;
	default:
		assert(false);
	}
	return strbuilder_build(b);
}

struct number_value *
number_value_copy(struct number_value *v)
{
	switch (v->type) {
	case NUMBER_VALUE_CONSTANT:
		return number_value_constant_create(v->constant);
	case NUMBER_VALUE_LIMIT:
		return number_value_limit_create(v->max);
	default:
		assert(false);
	}
}

bool
number_values_aresingle(struct number_value *v1, struct number_value *v2)
{
	/* XXX: this omits the case where we have a constant value equal to one
	 * of the limits */
	if (v1->type != v2->type) {
		return false;
	}
	switch (v1->type) {
	case NUMBER_VALUE_CONSTANT:
		return v1->constant == v2->constant-1;
	case NUMBER_VALUE_LIMIT:
		return v1->max == v2->max;
	default:
		assert(false);
	}
}

int
number_value_difference(struct number_value *v1, struct number_value *v2)
{
	assert(v1->type == v2->type);

	switch (v1->type) {
	case NUMBER_VALUE_CONSTANT:
		return v1->constant - v2->constant;
	default:
		assert(false);
	}
}

bool
number_value_equal(struct number_value *v1, struct number_value *v2)
{
	if (v1->type != v2->type) {
		return false;
	}
	switch (v1->type) {
	case NUMBER_VALUE_CONSTANT:
		return number_value_difference(v1, v2) == 0;
	case NUMBER_VALUE_LIMIT:
		return v1->max == v2->max;
	default:
		assert(false);
	}

}

int
number_value_as_constant(struct number_value *v)
{
	assert(v->type == NUMBER_VALUE_CONSTANT);

	return v->constant;
}

static bool
number_value_le_constant(struct number_value *v, int constant)
{
	switch (v->type) {
	case NUMBER_VALUE_CONSTANT:
		return v->constant <= constant;
	case NUMBER_VALUE_LIMIT:
		return !v->max;
	default:
		assert(false);
	}
}

static bool
constant_le_number_value(int constant, struct number_value *v)
{
	switch (v->type) {
	case NUMBER_VALUE_CONSTANT:
		return constant <= v->constant;
	case NUMBER_VALUE_LIMIT:
		return v->max;
	default:
		assert(false);
	}

}
