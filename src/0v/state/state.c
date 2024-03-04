#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "block.h"
#include "ext.h"
#include "heap.h"
#include "location.h"
#include "object.h"
#include "props.h"
#include "stack.h"
#include "state.h"
#include "util.h"
#include "value.h"

struct state {
	struct externals *ext;
	struct vconst *vconst;
	struct stack *stack;
	struct heap *heap;
	struct props *props;
};

struct state *
state_create(char *func, struct externals *ext, struct ast_type *result_type)
{
	struct state *state = malloc(sizeof(struct state));
	assert(state);
	state->ext = ext;
	state->vconst = vconst_create();
	state->stack = stack_create(func, NULL, result_type);
	state->heap = heap_create();
	state->props = props_create();
	return state;
}

void
state_destroy(struct state *state)
{
	/* vconst_destroy(state->vconst); */
	stack_destroy(state->stack);
	heap_destroy(state->heap);
	props_destroy(state->props);
	free(state);
}

struct state *
state_copy(struct state *state)
{
	struct state *copy = malloc(sizeof(struct state));
	assert(copy);
	copy->ext = state->ext;
	copy->vconst = vconst_copy(state->vconst);
	copy->stack = stack_copy(state->stack);
	copy->heap = heap_copy(state->heap);
	copy->props = props_copy(state->props);
	return copy;
}

struct state *
state_copywithname(struct state *state, char *func_name)
{
	struct state *copy = malloc(sizeof(struct state));
	assert(copy);
	copy->ext = state->ext;
	copy->vconst = vconst_copy(state->vconst);
	copy->stack = stack_copywithname(state->stack, func_name);
	copy->heap = heap_copy(state->heap);
	copy->props = props_copy(state->props);
	return copy;
}

char *
state_str(struct state *state)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "[[\n");
	char *ext = externals_types_str(state->ext, "\t");
	if (strlen(ext) > 0) {
		strbuilder_printf(b, "%s\n", ext);
	}
	free(ext);
	char *vconst = vconst_str(state->vconst, "\t");
	if (strlen(vconst) > 0) {
		strbuilder_printf(b, "%s\n", vconst);
	}
	free(vconst);
	char *stack = stack_str(state->stack, state);
	strbuilder_printf(b, "%s\n", stack);
	free(stack);
	char *props = props_str(state->props, "\t");
	if (strlen(props) > 0) {
		strbuilder_printf(b, "%s", props);
	}
	free(props);
	char *heap = heap_str(state->heap, "\t");
	if (strlen(heap) > 0) {
		strbuilder_printf(b, "\n%s\n", heap);
	}
	free(heap);
	strbuilder_printf(b, "]]\n");
	return strbuilder_build(b);
}

struct externals *
state_getext(struct state *s)
{
	return s->ext;
}

struct heap *
state_getheap(struct state *s)
{
	return s->heap;
}

struct props *
state_getprops(struct state *s)
{
	return s->props;
}

void
state_pushframe(struct state *state, char *func, struct ast_type *ret_type)
{
	state->stack = stack_create(func, state->stack, ret_type);
}

void
state_popframe(struct state *state)
{
	struct stack *old = state->stack;
	state->stack = stack_prev(old); /* pop */
	assert(state->stack);
	/* destroy old frame */
	stack_destroy(old);
}

void
state_declare(struct state *state, struct ast_variable *var, bool isparam)
{
	stack_declare(state->stack, var, isparam);
}

struct value *
state_vconst(struct state *state, struct ast_type *t, char *comment, bool persist)
{
	struct value *v = ast_type_vconst(t, state, comment, persist);
	if (value_isstruct(v)) {
		return v;
	}
	char *c = vconst_declare(
		state->vconst, v,
		comment, persist
	);
	return value_sync_create(ast_expr_identifier_create(c));
}

struct value *
state_getvconst(struct state *state, char *id)
{
	return vconst_get(state->vconst, id);
}

struct object *
state_get(struct state *state, struct location *loc, bool constructive)
{
	struct block *b = location_getblock(
		loc, state->vconst, state->stack, state->heap
	);
	if (!b) {
		assert(location_type(loc) == LOCATION_DYNAMIC);
		return NULL;
	}
	return block_observe(b, location_offset(loc), state, constructive);
}

struct block *
state_getblock(struct state *state, struct location *loc)
{
	return location_getblock(
		loc, state->vconst, state->stack, state->heap
	);
}

struct object *
state_getresult(struct state *state)
{
	struct variable *v = stack_getresult(state->stack);
	assert(v);

	return state_get(state, variable_location(v), true);
}

static struct ast_type *
state_getresulttype(struct state *state)
{
	struct variable *v = stack_getresult(state->stack);
	assert(v);

	return variable_type(v);
}

struct ast_type *
state_getobjecttype(struct state *state, char *id)
{
	if (strcmp(id, KEYWORD_RESULT) == 0) {
		return state_getresulttype(state);
	}

	struct variable *v = stack_getvariable(state->stack, id);
	assert(v);

	return variable_type(v);
}

struct value *
state_getloc(struct state *state, char *id)
{
	struct variable *v = stack_getvariable(state->stack, id);
	assert(v);

	return value_ptr_create(variable_location(v));
}

struct object *
state_getobject(struct state *state, char *id)
{
	if (strcmp(id, KEYWORD_RESULT) == 0) {
		return state_getresult(state);
	}

	struct variable *v = stack_getvariable(state->stack, id);
	if (!v) {
		assert(false);
	}

	return state_get(state, variable_location(v), true);
}


struct object *
state_deref(struct state *state, struct value *ptr_val, struct ast_expr *index)
{
	if (value_issync(ptr_val)) {
		return NULL;
	}
	struct location *deref_base = value_as_location(ptr_val);
	assert(deref_base);

	/* `*(ptr+offset)` */
	struct location *deref = location_with_offset(deref_base, index);
	struct object *res = state_get(state, deref, true);
	/*location_destroy(deref);*/
	return res;
}

struct error *
state_range_alloc(struct state *state, struct object *obj,
		struct ast_expr *lw, struct ast_expr *up)
{
	/* loc corresponds to, say, `arr`, so we dereference */
	struct value *arr_val = object_as_value(obj);
	if (!arr_val) {
		return error_create("no value");
	}

	/* assume pointer */
	struct location *deref = value_as_location(arr_val);

	struct block *b = location_getblock(
		deref, state->vconst, state->stack, state->heap
	);
	if (!b) {
		return error_create("no block");
	}

	/* TODO: prevent creation of virtual block for empty ranges */ 
	assert(!ast_expr_equal(lw, up));

	/* virtual block to represents range of values allocated */
	return block_range_alloc(b, lw, up, state->heap);
}

struct value *
state_alloc(struct state *state)
{
	return value_ptr_create(heap_newblock(state->heap));
}

struct error *
state_dealloc(struct state *state, struct value *val)
{
	if (!value_islocation(val)) {
		return error_create("dealloc on non-location");
	}
	return location_dealloc(value_as_location(val), state->heap);
}


struct error *
state_range_dealloc(struct state *state, struct object *obj,
		struct ast_expr *lw, struct ast_expr *up)
{
	/* obj corresponds to, say, `arr`, so we dereference */
	struct value *arr_val = object_as_value(obj);
	if (!arr_val) {
		return error_create("no value");
	}
	struct location *deref = value_as_location(arr_val);
	return location_range_dealloc(deref, lw, up, state);
}

bool
state_addresses_deallocand(struct state *state, struct object *obj)
{
	struct value *val = object_as_value(obj);
	struct location *loc = value_as_location(val); 
	
	return state_isdeallocand(state, loc);
}

bool
state_isdeallocand(struct state *s, struct location *loc)
{
	bool type_equal = location_type(loc) == LOCATION_DYNAMIC;
	struct block *b = state_getblock(s, loc);
	return type_equal && b;
}

bool
state_range_aredeallocands(struct state *state, struct object *obj,
		struct ast_expr *lw, struct ast_expr *up)
{
	/* XXX: for invariant initial conditions */
	if (ast_expr_equal(lw, up)) {
		return true;
	}
	/* obj corresponds to, say, `arr`, so we dereference */
	struct value *arr_val = object_as_value(obj);
	if (!arr_val) {
		return false;
	}
	struct location *deref = value_as_location(arr_val);
	
	struct block *b = location_getblock(
		deref, state->vconst, state->stack, state->heap
	);
	return (bool) b && block_range_aredeallocands(b, lw, up, state);
}

bool
state_hasgarbage(struct state *state)
{
	return !heap_referenced(state->heap, state);
}

void
state_location_destroy(struct location *loc)
{
	location_destroy(loc);
}

bool
state_references(struct state *s, struct location *loc)
{
	return stack_references(s->stack, loc, s);
}

bool
state_eval(struct state *s, struct ast_expr *e)
{
	return vconst_eval(s->vconst, e);
}

static void
state_undeclarevars(struct state *s);

static void
state_popprops(struct state *s);

bool
state_equal(struct state *s1, struct state *s2)
{
	struct state *s1_c = state_copy(s1),
		     *s2_c = state_copy(s2);
	state_undeclarevars(s1_c);
	state_undeclarevars(s2_c);
	state_popprops(s1_c);
	state_popprops(s2_c);

	char *str1 = state_str(s1_c),
	     *str2 = state_str(s2_c);
	bool equal = strcmp(str1, str2) == 0;
	if (!equal) {
		printf("actual: %s\n", str1);
		printf("alleged: %s\n", str2);
	}
	free(str2);
	free(str1);

	state_destroy(s2_c);
	state_destroy(s1_c);

	return equal;
}

static void
state_undeclarevars(struct state *s)
{
	heap_undeclare(s->heap, s);
	vconst_undeclare(s->vconst);
	stack_undeclare(s->stack, s);
}

static void
state_popprops(struct state *s)
{
	props_destroy(s->props);
	s->props = props_create();
}
