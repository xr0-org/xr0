#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ext.h"
#include "ast.h"
#include "state.h"
#include "object.h"
#include "util.h"
#include "value.h"

static struct range *
range_copy(struct range *);

static void
range_destroy(struct range *);

static char *
range_str(struct range *);

static struct ast_expr *
range_size(struct range *);

static struct error *
range_dealloc(struct range *r, struct state *s);

static bool
range_isdeallocand(struct range *, struct state *);

static bool
range_references(struct range *, struct location *, struct state *,
		struct circuitbreaker *cb);

static struct range * 
range_permuteheaplocs(struct range *, struct permutation *);

static struct int_arr *
range_deriveorder(struct range *, struct circuitbreaker *, struct state *);

struct object {
	enum object_type {
		OBJECT_VALUE, OBJECT_DEALLOCAND_RANGE,
	} type;
	struct ast_expr *offset;
	union {
		struct range *range;
		struct value *value;
	};
};

struct object *
object_value_create(struct ast_expr *offset, struct value *v)
{
	struct object *obj = malloc(sizeof(struct object));
	assert(obj);
	obj->offset = offset;
	obj->value = v;
	obj->type = OBJECT_VALUE;
	return obj;
}

struct object *
object_range_create(struct ast_expr *offset, struct range *r)
{
	assert(r);
	struct object *obj = malloc(sizeof(struct object));
	assert(obj);
	obj->offset = offset;
	obj->range = r;
	obj->type = OBJECT_DEALLOCAND_RANGE;
	return obj;
}

struct int_arr *
object_deriveorder(struct object *obj, struct circuitbreaker *cb, struct state *s)
{
	switch (obj->type) {
	case OBJECT_VALUE:
		return obj->value
			? value_deriveorder(obj->value, cb, s)
			: int_arr_create();
	case OBJECT_DEALLOCAND_RANGE:
		return range_deriveorder(obj->range, cb, s);
	default:
		assert(false);
	}
}

struct object *
object_permuteheaplocs(struct object *old, struct permutation *p)
{
	struct object *new = malloc(sizeof(struct object));
	new->offset = ast_expr_copy(old->offset);
	new->type = old->type;
	switch (old->type) {
	case OBJECT_VALUE:
		new->value = old->value
			? value_permuteheaplocs(old->value, p)
			: NULL;
		break;
	case OBJECT_DEALLOCAND_RANGE:
		new->range = range_permuteheaplocs(old->range, p);
		break;
	default:
		assert(false);
	}
	return new;
}

void
object_destroy(struct object *obj)
{
	switch (obj->type) {
	case OBJECT_VALUE:
		if (obj->value) {
			value_destroy(obj->value);
		}
		break;
	case OBJECT_DEALLOCAND_RANGE:
		range_destroy(obj->range);
		break;
	default:
		assert(false);
	}
	ast_expr_destroy(obj->offset);
	free(obj);
}

struct object *
object_copy(struct object *old)
{
	struct object *new = malloc(sizeof(struct object));
	new->offset = ast_expr_copy(old->offset);
	new->type = old->type;
	switch (old->type) {
	case OBJECT_VALUE:
		new->value = old->value ?  value_copy(old->value) : NULL;
		break;
	case OBJECT_DEALLOCAND_RANGE:
		new->range = range_copy(old->range);
		break;
	default:
		assert(false);
	}
	return new;
}

struct object *
object_abstractcopy(struct object *old, struct state *s)
{
	switch (old->type) {
	case OBJECT_DEALLOCAND_RANGE:
		/* TODO: reset observations */
		return object_copy(old);
	case OBJECT_VALUE:
		return object_value_create(
			ast_expr_copy(old->offset),
			old->value ? value_abstractcopy(old->value, s) : NULL
		);
	default:
		assert(false);
	}
}

static char *
inner_str(struct object *);

char *
object_str(struct object *obj)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "{");
	char *offset = ast_expr_str(obj->offset);
	strbuilder_printf(b, "%s:", offset);
	free(offset);
	char *inner = inner_str(obj);
	strbuilder_printf(b, "<%s>", inner);
	free(inner);
	strbuilder_printf(b, "}");
	return strbuilder_build(b);
}

static char *
inner_str(struct object *obj)
{
	switch (obj->type) {
	case OBJECT_VALUE:
		return obj->value ? value_str(obj->value) : dynamic_str("");
	case OBJECT_DEALLOCAND_RANGE:
		return range_str(obj->range);
	default:
		assert(false);
	}
}

bool
object_referencesheap(struct object *obj, struct state *s,
		struct circuitbreaker *cb)
{
	if (!circuitbreaker_append(cb, obj)) {
		/* already analysed */
		return false;
	}
	if (!object_isvalue(obj)) {
		/* TODO: do we need to exclude the case of ranges on stack? */
		return true;
	}
	struct circuitbreaker *copy = circuitbreaker_copy(cb);
	bool ans = obj->value && value_referencesheap(obj->value, s, copy);
	circuitbreaker_destroy(copy);
	return ans;
}

bool
object_hasvalue(struct object *obj)
{
	if (object_isvalue(obj)) {
		return obj->value;
	}
	return false;
}

bool
object_isvalue(struct object *obj)
{
	return obj->type == OBJECT_VALUE;
}

struct value *
object_as_value(struct object *obj)
{
	assert(obj->type == OBJECT_VALUE);

	return obj->value;
}

bool
object_isdeallocand(struct object *obj, struct state *s)
{
	switch (obj->type) {
	case OBJECT_VALUE:
		return obj->value && state_isdeallocand(s, value_as_location(obj->value));
	case OBJECT_DEALLOCAND_RANGE:
		return range_isdeallocand(obj->range, s);
	default:
		assert(false);
	}
}

bool
object_references(struct object *obj, struct location *loc, struct state *s,
		struct circuitbreaker *cb)
{
	if (!circuitbreaker_append(cb, obj)) {
		/* already handled */
		return false;
	}
	if (obj->type == OBJECT_DEALLOCAND_RANGE) {
		return range_references(obj->range, loc, s, cb);
	}

	assert(obj->type == OBJECT_VALUE);

	struct circuitbreaker *copy = circuitbreaker_copy(cb);
	struct value *v = object_as_value(obj);
	bool ans = v ? value_references(v, loc, s, copy) : false;
	circuitbreaker_destroy(copy);
	return ans;
}

struct error *
object_assign(struct object *obj, struct value *val)
{
	assert(obj->type == OBJECT_VALUE);

	/* XXX: check that if val has offset it's within range and return error
	 * potentially */

	obj->value = val;
	return NULL;
}

static struct ast_expr *
object_size(struct object *obj)
{
	switch (obj->type) {
	case OBJECT_VALUE:
		/* TODO: derive properly from value type */
		return ast_expr_constant_create(1);
	case OBJECT_DEALLOCAND_RANGE:
		return ast_expr_copy(range_size(obj->range));
	default:
		assert(false);
	}
}

struct ast_expr *
object_lower(struct object *obj)
{
	return obj->offset;
}

struct ast_expr *
object_upper(struct object *obj)
{
	return ast_expr_sum_create(
		ast_expr_copy(obj->offset),
		object_size(obj)
	);
}

bool
object_contains(struct object *obj, struct ast_expr *offset, struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj),
			*of = offset;

	struct ast_expr *e1 = ast_expr_le_create(
		ast_expr_copy(lw), ast_expr_copy(of)
	);
	struct ast_expr *e2 = ast_expr_lt_create(
		ast_expr_copy(of), ast_expr_copy(up)
	);
	ast_expr_destroy(up);
	
	bool contains =
		/* lw ≤ of */
		state_eval(s, e1)
		&&
		/* of < up */
		state_eval(s, e2);

	ast_expr_destroy(e2);
	ast_expr_destroy(e1);

	return contains;
}

bool
object_contains_upperincl(struct object *obj, struct ast_expr *offset,
		struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj),
			*of = offset;

	return
		/* lw ≤ of */
		state_eval(s, ast_expr_le_create(lw, of))

		&&

		/* of ≤ up */
		state_eval(s, ast_expr_le_create(of, up));
}

bool
object_isempty(struct object *obj, struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj);

	return state_eval(s, ast_expr_eq_create(lw, up));
}

bool
object_contig_precedes(struct object *before, struct object *after,
		struct state *s)
{
	struct ast_expr *lw = object_upper(before),
			*up = after->offset;
	return state_eval(s, ast_expr_eq_create(lw, up));
}

bool
object_issingular(struct object *obj, struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj);

	struct ast_expr *lw_succ = ast_expr_sum_create(
		lw, ast_expr_constant_create(1)
	);

	/* lw + 1 == up */
	return state_eval(s, ast_expr_eq_create(lw_succ, up));
}

struct object *
object_upto(struct object *obj, struct ast_expr *excl_up, struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj);

	struct ast_expr *prop0 = ast_expr_le_create(
		ast_expr_copy(lw), ast_expr_copy(excl_up)
	);
	struct ast_expr *prop1 = ast_expr_eq_create(
		ast_expr_copy(lw), ast_expr_copy(excl_up)
	);
	struct ast_expr *prop2 = ast_expr_eq_create(
		ast_expr_copy(up), ast_expr_copy(excl_up)
	);

	bool e0 = state_eval(s, prop0),
	     e1 = state_eval(s, prop1),
	     e2 = state_eval(s, prop2);

	ast_expr_destroy(prop2);
	ast_expr_destroy(prop1);
	ast_expr_destroy(prop0);
	ast_expr_destroy(up);

	assert(e0);

	if (e1) {
		return NULL;
	}

	if (e2) {
		assert(obj->type == OBJECT_VALUE);
		return object_value_create(
			ast_expr_copy(obj->offset), value_copy(obj->value)
		);
	}
	return object_range_create(
		ast_expr_copy(obj->offset),
		range_create(
			ast_expr_difference_create(excl_up, lw),
			value_as_location(state_alloc(s))
		)
	);
}

struct object *
object_from(struct object *obj, struct ast_expr *incl_lw, struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj);

	struct ast_expr *prop0 = ast_expr_ge_create(
		ast_expr_copy(incl_lw), ast_expr_copy(up)
	);
	struct ast_expr *prop1 = ast_expr_eq_create(
		ast_expr_copy(incl_lw), ast_expr_copy(lw)
	);

	bool e0 = state_eval(s, prop0),
	     e1 = state_eval(s, prop1);

	ast_expr_destroy(prop1);
	ast_expr_destroy(prop0);

	if (e0) {
		ast_expr_destroy(up);
		return NULL;
	}

	if (e1) {
		assert(obj->type == OBJECT_VALUE);
		ast_expr_destroy(up);
		return object_value_create(
			ast_expr_copy(incl_lw),
			value_copy(obj->value)
		);
	}
	return object_range_create(
		ast_expr_copy(incl_lw),
		range_create(
			ast_expr_difference_create(
				up,
				ast_expr_copy(incl_lw)
			),
			value_as_location(state_alloc(s))
		)
	);
}


struct error *
object_dealloc(struct object *obj, struct state *s)
{
	switch (obj->type) {
	case OBJECT_VALUE:
		return state_dealloc(s, obj->value);
	case OBJECT_DEALLOCAND_RANGE:
		return range_dealloc(obj->range, s);
	default:
		assert(false);
	}
}

static struct value *
getorcreatestruct(struct object *, struct ast_type *, struct state *);

struct object *
object_getmember(struct object *obj, struct ast_type *t, char *member,
		struct state *s)
{
	return value_struct_member(getorcreatestruct(obj, t, s), member);
}

static struct value *
getorcreatestruct(struct object *obj, struct ast_type *t, struct state *s)
{
	struct value *v = object_as_value(obj);
	if (v) {
		return v;
	}
	struct ast_type *complete = ast_type_struct_complete(t, state_getext(s));
	a_printf(complete, "%s is incomplete\n", ast_type_str(t));
	v = value_struct_create(complete);
	object_assign(obj, v);
	return v;
}

struct ast_type *
object_getmembertype(struct object *obj, struct ast_type *t, char *member,
		struct state *s)
{
	return value_struct_membertype(
		getorcreatestruct(obj, t, s), member
	);
}

struct object_result {
	struct object *val;
	struct error *err;
};


struct object_result *
object_result_error_create(struct error *err)
{
	assert(err);

	struct object_result *r = malloc(sizeof(struct object_result));
	r->val = NULL;
	r->err = err;
	return r;
}

struct object_result *
object_result_value_create(struct object *val)
{
	struct object_result *r = malloc(sizeof(struct object_result));
	r->val = val;
	r->err = NULL;
	return r;
}

void
object_result_destroy(struct object_result *res)
{
	assert(!res->err);
	if (res->val) {
		object_destroy(res->val);
	}
	free(res);
}

bool
object_result_iserror(struct object_result *res)
{
	return res->err;
}

struct error *
object_result_as_error(struct object_result *res)
{
	assert(res->err);
	return res->err;
}

struct object *
object_result_as_value(struct object_result *res)
{
	assert(!res->err);
	return res->val;
}

bool
object_result_hasvalue(struct object_result *res)
{
	assert(!object_result_iserror(res));
	return res->val; /* implicit cast */
}

struct range {
	struct ast_expr *size;
	struct location *loc;
};

struct range *
range_create(struct ast_expr *size, struct location *loc)
{
	struct range *r = malloc(sizeof(struct range));
	r->size = size;
	r->loc = loc;
	return r;
}

struct range *
range_copy(struct range *r)
{
	return range_create(ast_expr_copy(r->size), location_copy(r->loc));
}

static struct range * 
range_permuteheaplocs(struct range *r, struct permutation *p)
{
	return range_create(
		ast_expr_copy(r->size), location_permuteheap(r->loc, p)
	);
}

static struct int_arr *
range_deriveorder(struct range *r, struct circuitbreaker *cb, struct state *s)
{
	assert(false);
}

void
range_destroy(struct range *r)
{
	ast_expr_destroy(r->size);
	location_destroy(r->loc);
	free(r);
}

char *
range_str(struct range *r)
{
	struct strbuilder *b = strbuilder_create();
	char *size = ast_expr_str(r->size);
	char *loc = location_str(r->loc);
	strbuilder_printf(b, "virt:%s@%s", size, loc);
	free(loc);
	free(size);
	return strbuilder_build(b);
}

struct ast_expr *
range_size(struct range *r)
{
	return r->size;
}

struct error *
range_dealloc(struct range *r, struct state *s)
{
	return state_dealloc(s, value_ptr_create(r->loc));
}

bool
range_isdeallocand(struct range *r, struct state *s)
{
	return state_isdeallocand(s, r->loc);
}

bool
range_references(struct range *r, struct location *loc, struct state *s,
		struct circuitbreaker *cb)
{
	return location_references(r->loc, loc, s, cb);
}


struct object_arr {
	int n;
	struct object **object;
};

struct object_arr *
object_arr_create()
{
	struct object_arr *arr = calloc(1, sizeof(struct object_arr));
	assert(arr);
	return arr;
}

void
object_arr_destroy(struct object_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		object_destroy(arr->object[i]);
	}
	free(arr->object);
	free(arr);
}

int
object_arr_append(struct object_arr *arr, struct object *obj);

struct object_arr *
object_arr_copy(struct object_arr *arr)
{
	struct object_arr *copy = object_arr_create();
	for (int i = 0; i < arr->n; i++) {
		object_arr_append(copy, object_copy(arr->object[i]));
	}
	return copy;
}

struct object **
object_arr_allocs(struct object_arr *arr)
{
	return arr->object;
}

int
object_arr_nallocs(struct object_arr *arr)
{
	return arr->n;
}

int
object_arr_index(struct object_arr *arr, struct ast_expr *offset,
		struct state *state)
{
	for (int i = 0; i < arr->n; i++) {
		if (object_contains(arr->object[i], offset, state)) {
			return i;
		}
	}
	return -1;
}

int
object_arr_index_upperincl(struct object_arr *arr, struct ast_expr *offset,
		struct state *state)
{
	for (int i = 0; i < arr->n; i++) {
		if (object_contains_upperincl(arr->object[i], offset, state)) {
			return i;
		}
	}
	return -1;
}

int
object_arr_insert(struct object_arr *arr, int index, struct object *obj)
{
	/*assert(!object_isempty(obj));*/

	arr->object = realloc(arr->object, sizeof(struct object *) * ++arr->n);
	assert(arr->object);
	for (int i = arr->n-1; i > index; i--) {
		arr->object[i] = arr->object[i-1];
	}
	arr->object[index] = obj;
	return index;
}

int
object_arr_append(struct object_arr *arr, struct object *obj)
{
	return object_arr_insert(arr, arr->n, obj);
}

void
object_arr_remove(struct object_arr *arr, int index)
{
	for (int i = index; i < arr->n-1; i++) {
		arr->object[i] = arr->object[i+1];
	}
	if (arr->n == 1) {
		--arr->n;
		free(arr->object[0]);
	} else {
		arr->object = realloc(arr->object, sizeof(struct object *) * --arr->n);
		assert(arr->object || !arr->n);
	}
}

int
object_arr_nobjects(struct object_arr *arr)
{
	return arr->n;
}

struct object **
object_arr_objects(struct object_arr *arr)
{
	return arr->object;
}
