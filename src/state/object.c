#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "heap.h"
#include "location.h"
#include "value.h"
#include "util.h"

struct object {
	struct ast_expr *lower, *upper;
	struct value *value;
};

struct object *
object_create(struct ast_expr *lower, struct ast_expr *upper)
{
	struct object *obj = malloc(sizeof(struct object));
	assert(obj);
	obj->lower = lower;
	obj->upper = upper;
	obj->value = NULL;
	return obj;
}

void
object_destroy(struct object *obj)
{
	ast_expr_destroy(obj->lower);
	ast_expr_destroy(obj->upper);
	if (obj->value) {
		value_destroy(obj->value);
	}
	free(obj);
}

char *
object_str(struct object *obj)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "[");
	char *lw = ast_expr_str(obj->lower),
	     *up = ast_expr_str(obj->upper);
	strbuilder_printf(b, "%s:%s:", lw, up);
	free(up);
	free(lw);
	if (obj->value) {
		char *val = value_str(obj->value);
		strbuilder_printf(b, "<%s>", val);
		free(val);
	} else {
		strbuilder_printf(b, "<>");
	}
	strbuilder_printf(b, "]");
	return strbuilder_build(b);
}

struct ast_expr *
object_lower(struct object *obj)
{
	assert(obj);
	return obj->lower;
}

struct ast_expr *
object_upper(struct object *obj)
{
	assert(obj);
	return obj->upper;
}

struct value *
object_value(struct object *obj)
{
	assert(obj);
	return obj->value;
}

void
object_assign(struct object *obj, struct value *val)
{
	assert(obj);

	/* XXX: check that if val has offset it's within range and return error
	 * potentially */

	obj->value = val;
}

bool
hack_is_i_in_i_to_i_plus_one(struct object *obj, struct ast_expr *offset);

struct ast_expr *
calc_simplify(struct ast_expr *expr);

bool
object_contains(struct object *obj, struct ast_expr *offset)
{
	if (hack_is_i_in_i_to_i_plus_one(obj, offset)) {
		return true;
	}

	struct ast_expr *simp_lw = calc_simplify(obj->lower),
			*simp_up = calc_simplify(obj->upper),
			*simp_of = calc_simplify(offset);

	int lw = ast_expr_as_constant(simp_lw),
	    up = ast_expr_as_constant(simp_up),
	    of = ast_expr_as_constant(simp_of);

	ast_expr_destroy(simp_of);
	ast_expr_destroy(simp_up);
	ast_expr_destroy(simp_lw);

	return lw <= of && of < up;
}

bool
hack_is_i_in_i_to_i_plus_one(struct object *obj, struct ast_expr *offset)
{
	if (ast_expr_kind(offset) != EXPR_IDENTIFIER) {
		return false;
	}
	if (strcmp(ast_expr_as_identifier(offset), "i") != 0) {
		return false;
	}
	if (!ast_expr_equal(obj->lower, offset)) {
		return false;
	}
	struct ast_expr *up = obj->upper;
	if (ast_expr_kind(up) != EXPR_BINARY) {
		return false;
	}
	struct ast_expr *e1 = ast_expr_binary_e1(up),
			*e2 = ast_expr_binary_e2(up);
	if (ast_expr_kind(e2) != EXPR_CONSTANT) {
		return false;
	}
	return ast_expr_equal(e1, offset)
		&& ast_expr_binary_op(up) == BINARY_OP_ADDITION
		&& ast_expr_as_constant(e2) == 1;
}

static struct ast_expr *
binary_simplify(struct ast_expr *expr);

struct ast_expr *
calc_simplify(struct ast_expr *expr)
{
	switch (ast_expr_kind(expr)) {
	case EXPR_CONSTANT:
	case EXPR_IDENTIFIER:
		return ast_expr_copy(expr);
	case EXPR_BINARY:
		return binary_simplify(expr);
	default:
		assert(false);
	}
}

static struct ast_expr *
binary_simplify_memory_safe_decision(struct ast_expr *expr, struct ast_expr *e1,
		struct ast_expr *e2);

static struct ast_expr *
binary_simplify(struct ast_expr *expr)
{
	struct ast_expr *e1 = calc_simplify(ast_expr_binary_e1(expr)),
			*e2 = calc_simplify(ast_expr_binary_e2(expr));
	assert(ast_expr_binary_op(expr) == BINARY_OP_ADDITION);
	struct ast_expr *s = binary_simplify_memory_safe_decision(expr, e1, e2);	
	ast_expr_destroy(e1);
	ast_expr_destroy(e2);
	return s;
}

static struct ast_expr *
binary_simplify_memory_safe_decision(struct ast_expr *expr, struct ast_expr *e1,
		struct ast_expr *e2)
{
	if (ast_expr_kind(e1) != ast_expr_kind(e2)) { /* XXX */
		return ast_expr_copy(expr);
	}
	return ast_expr_create_constant(
		ast_expr_as_constant(e1) + ast_expr_as_constant(e2)
	);
}

bool
hack_is_i_plus_one_in_i_to_i_plus_one(struct object *obj, struct ast_expr *offset);

bool
object_contains_upperincl(struct object *obj, struct ast_expr *offset)
{
	if (hack_is_i_plus_one_in_i_to_i_plus_one(obj, offset)) {
		return true;
	}

	struct ast_expr *simp_lw = calc_simplify(obj->lower),
			*simp_up = calc_simplify(obj->upper),
			*simp_of = calc_simplify(offset);

	int lw = ast_expr_as_constant(simp_lw),
	    up = ast_expr_as_constant(simp_up),
	    of = ast_expr_as_constant(simp_of);

	ast_expr_destroy(simp_of);
	ast_expr_destroy(simp_up);
	ast_expr_destroy(simp_lw);

	return lw <= of && of <= up;
}

bool
hack_is_i_plus_one_in_i_to_i_plus_one(struct object *obj, struct ast_expr *offset)
{
	/* lower is `i' */
	struct ast_expr *lw = obj->lower;
	if (ast_expr_kind(lw) != EXPR_IDENTIFIER) {
		return false;
	}
	if (strcmp(ast_expr_as_identifier(lw), "i") != 0) {
		return false;
	}

	/* offset, upper are both `i+1' */
	if (!ast_expr_equal(offset, obj->upper)) {
		return false;
	}
	if (ast_expr_kind(offset) != EXPR_BINARY) {
		return false;
	}
	struct ast_expr *e1 = ast_expr_binary_e1(offset),
			*e2 = ast_expr_binary_e2(offset);
	if (ast_expr_kind(e2) != EXPR_CONSTANT) {
		return false;
	}
	return ast_expr_equal(e1, lw)
		&& ast_expr_binary_op(offset) == BINARY_OP_ADDITION
		&& ast_expr_as_constant(e2) == 1;
}

bool
object_isempty(struct object *obj)
{
	struct ast_expr *lw = calc_simplify(obj->lower),
			*up = calc_simplify(obj->upper);
	bool empty = ast_expr_equal(lw, up);
	ast_expr_destroy(lw);
	ast_expr_destroy(up);
	return empty;
}

bool
object_contig_precedes(struct object *before, struct object *after)
{
	if (ast_expr_equal(before->upper, after->lower)) {
		return true;
	}
	return false;
}

bool
object_isvirtual(struct object *obj)
{
	assert(obj);
	struct value *val = object_value(obj);
	if (!val) {
		return false;
	}
	return (bool) value_type(val) == VALUE_VIRTUAL;
}

bool
hack_is_i_to_i_plus_one(struct object *);

bool
object_issingular(struct object *obj)
{
	if (hack_is_i_to_i_plus_one(obj)) {
		return true;
	}
	struct ast_expr *simp_lw = calc_simplify(obj->lower),
			*simp_up = calc_simplify(obj->upper);
	int lw = ast_expr_as_constant(simp_lw),
	    up = ast_expr_as_constant(simp_up);
	bool singular = up-lw == 1;
	ast_expr_destroy(simp_lw);
	ast_expr_destroy(simp_up);
	return singular;
}

bool
hack_is_i_to_i_plus_one(struct object *obj)
{
	if (ast_expr_kind(obj->lower) != EXPR_IDENTIFIER) {
		return false;
	}
	if (strcmp(ast_expr_as_identifier(obj->lower), "i") != 0) {
		return false;
	}
	struct ast_expr *up = obj->upper;
	if (ast_expr_kind(up) != EXPR_BINARY) {
		return false;
	}
	struct ast_expr *e1 = ast_expr_binary_e1(up),
			*e2 = ast_expr_binary_e2(up);
	if (ast_expr_kind(e2) != EXPR_CONSTANT) {
		return false;
	}
	return ast_expr_equal(obj->lower, e1)
		&& ast_expr_binary_op(up) == BINARY_OP_ADDITION
		&& ast_expr_as_constant(e2) == 1;
}

struct object *
object_upto(struct object *obj, struct ast_expr *excl_upper, struct heap *h)
{
	if (hack_is_i_in_i_to_i_plus_one(obj, excl_upper)) {
		return NULL;
	}

	struct ast_expr *simp_lw = calc_simplify(obj->lower),
			*simp_up = calc_simplify(obj->upper),
			*simp_excl_up = calc_simplify(excl_upper);

	int lw = ast_expr_as_constant(simp_lw),
	    up = ast_expr_as_constant(simp_up),
	    excl_up = ast_expr_as_constant(simp_excl_up);

	ast_expr_destroy(simp_lw);
	ast_expr_destroy(simp_up);
	ast_expr_destroy(simp_excl_up);

	assert(excl_up <= up);

	if (lw == excl_up) {
		return NULL;
	}

	struct object *result = object_create(
		ast_expr_copy(obj->lower), ast_expr_copy(excl_upper)
	);
	if (up == excl_up) {
		object_assign(result, value_copy(obj->value));
	} else {
		object_assign(result, value_virt_create(heap_newblock(h, excl_up-lw)));
	}
	return result;
}

struct object *
object_from(struct object *obj, struct ast_expr *incl_lower, struct heap *h)
{
	if (hack_is_i_plus_one_in_i_to_i_plus_one(obj, incl_lower)) {
		return NULL;
	}

	struct ast_expr *simp_lw = calc_simplify(obj->lower),
			*simp_up = calc_simplify(obj->upper),
			*simp_incl_lw = calc_simplify(incl_lower);

	int lw = ast_expr_as_constant(simp_lw),
	    up = ast_expr_as_constant(simp_up),
	    incl_lw = ast_expr_as_constant(simp_incl_lw);

	ast_expr_destroy(simp_lw);
	ast_expr_destroy(simp_up);
	ast_expr_destroy(simp_incl_lw);

	assert(lw <= incl_lw);

	if (incl_lw == up) {
		return NULL;
	}

	struct object *result = object_create(
		ast_expr_copy(incl_lower), ast_expr_copy(obj->upper)
	);
	if (lw == incl_lw) {
		object_assign(result, value_copy(obj->value));
	} else {
		object_assign(result, value_virt_create(heap_newblock(h, up-incl_lw)));
	}
	return result;
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
object_arr_index(struct object_arr *arr, struct ast_expr *offset)
{
	for (int i = 0; i < arr->n; i++) {
		if (object_contains(arr->object[i], offset)) {
			return i;
		}
	}
	return -1;
}

int
object_arr_index_upperincl(struct object_arr *arr, struct ast_expr *offset)
{
	for (int i = 0; i < arr->n; i++) {
		if (object_contains_upperincl(arr->object[i], offset)) {
			return i;
		}
	}
	return -1;
}

void
object_arr_insert(struct object_arr *arr, int index, struct object *obj)
{
	assert(!object_isempty(obj));

	arr->object = realloc(arr->object, sizeof(struct object *) * ++arr->n);
	assert(arr->object);
	for (int i = arr->n-1; i > index; i--) {
		arr->object[i] = arr->object[i-1];
	}
	arr->object[index] = obj;
}

void
object_arr_append(struct object_arr *arr, struct object *obj)
{
	return object_arr_insert(arr, arr->n, obj);
}

void
object_arr_delete(struct object_arr *arr, int index)
{
	object_destroy(arr->object[index]);
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
