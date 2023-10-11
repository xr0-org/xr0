#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "heap.h"
#include "state.h"
#include "location.h"
#include "object.h"
#include "util.h"
#include "value.h"

struct range *
range_copy(struct range *);

void
range_destroy(struct range *);

char *
range_str(struct range *);

struct ast_expr *
range_size(struct range *);

struct error *
range_dealloc(struct range *r, struct state *s);

bool
range_isdeallocand(struct range *, struct state *);

bool
range_references(struct range *, struct location *, struct state *);


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
		return obj->value && state_isdeallocand(s, value_as_ptr(obj->value));
	case OBJECT_DEALLOCAND_RANGE:
		return range_isdeallocand(obj->range, s);
	default:
		assert(false);
	}
}

bool
object_references(struct object *obj, struct location *loc, struct state *s)
{
	if (obj->type == OBJECT_DEALLOCAND_RANGE) {
		return range_references(obj->range, loc, s);
	}

	assert(obj->type == OBJECT_VALUE);

	struct value *v = object_as_value(obj);
	return v ? value_references(v, loc, s) : false;
}

void
object_assign(struct object *obj, struct value *val)
{
	assert(obj->type == OBJECT_VALUE);

	/* XXX: check that if val has offset it's within range and return error
	 * potentially */

	obj->value = val;
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
	return ast_expr_binary_create(
		ast_expr_copy(obj->offset),
		BINARY_OP_ADDITION,
		object_size(obj)
	);
}

bool
object_contains(struct object *obj, struct ast_expr *offset, struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj),
			*of = offset;

	struct ast_expr *e1 = ast_expr_binary_create(
		ast_expr_copy(lw), BINARY_OP_LE, ast_expr_copy(of)
	);
	struct ast_expr *e2 = ast_expr_binary_create(
		ast_expr_copy(of), BINARY_OP_LT, ast_expr_copy(up)
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
		state_eval(s, ast_expr_binary_create(lw, BINARY_OP_LE, of))

		&&

		/* of ≤ up */
		state_eval(s, ast_expr_binary_create(of, BINARY_OP_LE, up));
}

bool
object_isempty(struct object *obj, struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj);

	return state_eval(s, ast_expr_binary_create(lw, BINARY_OP_EQ, up));
}

bool
object_contig_precedes(struct object *before, struct object *after,
		struct state *s)
{
	struct ast_expr *lw = object_upper(before),
			*up = after->offset;
	return state_eval(s, ast_expr_binary_create(lw, BINARY_OP_EQ, up));
}

bool
object_issingular(struct object *obj, struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj);

	struct ast_expr *lw_succ = ast_expr_binary_create(
		lw, BINARY_OP_ADDITION, ast_expr_constant_create(1)
	);

	/* lw + 1 == up */
	return state_eval(s, ast_expr_binary_create(lw_succ, BINARY_OP_EQ, up));
}

struct object *
object_upto(struct object *obj, struct ast_expr *excl_up, struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj);

	struct ast_expr *prop0 = ast_expr_binary_create(lw, BINARY_OP_LE, excl_up),
			*prop1 = ast_expr_binary_create(lw, BINARY_OP_EQ, excl_up),
			*prop2 = ast_expr_binary_create(up, BINARY_OP_EQ, excl_up);

	assert(state_eval(s, prop0));

	if (state_eval(s, prop1)) {
		return NULL;
	}

	if (state_eval(s, prop2)) {
		assert(obj->type == OBJECT_VALUE);
		return object_value_create(
			ast_expr_copy(obj->offset), value_copy(obj->value)
		);
	}
	return object_range_create(
		ast_expr_copy(obj->offset),
		range_create(
			ast_expr_binary_create(excl_up, BINARY_OP_SUBTRACTION, lw),
			value_as_ptr(state_alloc(s))
		)
	);
}

struct object *
object_from(struct object *obj, struct ast_expr *incl_lw, struct state *s)
{
	struct ast_expr *lw = obj->offset,
			*up = object_upper(obj);

	struct ast_expr *prop0 = ast_expr_binary_create(incl_lw, BINARY_OP_GE, up),
			*prop1 = ast_expr_binary_create(incl_lw, BINARY_OP_EQ, lw);

	if (state_eval(s, prop0)) {
		return NULL;
	}

	if (state_eval(s, prop1)) {
		assert(obj->type == OBJECT_VALUE);
		return object_value_create(
			ast_expr_copy(incl_lw),
			value_copy(obj->value)
		);
	}
	return object_range_create(
		ast_expr_copy(incl_lw),
		range_create(
			ast_expr_binary_create(ast_expr_copy(up), BINARY_OP_SUBTRACTION, ast_expr_copy(incl_lw)),
			value_as_ptr(state_alloc(s))
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
range_references(struct range *r, struct location *loc, struct state *s)
{
	return location_references(r->loc, loc, s);
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
	arr->object = realloc(arr->object, sizeof(struct alloc *) * --arr->n);
	assert(arr->object || !arr->n);
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
