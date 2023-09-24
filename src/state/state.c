#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "state.h"
#include "stack.h"
#include "heap.h"
#include "block.h"
#include "location.h"
#include "object.h"
#include "value.h"
#include "ast.h"
#include "util.h"

struct state {
	struct map *extfunc;
	struct stack *stack;
	struct heap *heap;
};

struct state *
state_create(struct map *extfunc, struct ast_type *result_type)
{
	struct state *state = malloc(sizeof(struct state));
	assert(state);
	state->extfunc = extfunc;
	state->stack = stack_create(NULL, result_type);
	state->heap = heap_create();
	return state;
}

void
state_destroy(struct state *state)
{
	stack_destroy(state->stack);
	heap_destroy(state->heap);
	free(state);
}

char *
state_str(struct state *state)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "[[\n");
	char *stack = stack_str(state->stack, state->heap);
	strbuilder_printf(b, "%s", stack);
	free(stack);
	char *heap = heap_str(state->heap, "\t");
	if (strlen(heap) > 0) {
		strbuilder_printf(b, "\n%s\n", heap);
	}
	free(heap);
	strbuilder_printf(b, "]]\n");
	return strbuilder_build(b);
}

struct map *
state_extfunc(struct state *state)
{
	return state->extfunc;
}

struct ast_function *
state_getfunc(struct state *state, char *f)
{
	struct ast_function *sym = map_get(state->extfunc, f);
	assert(sym);
	return sym;
}

void
state_pushframe(struct state *state, struct ast_type *ret_type)
{
	state->stack = stack_create(state->stack, ret_type);
}

void
state_popframe(struct state *state)
{
	struct stack *old = state->stack;
	state->stack = stack_prev(old, 1); /* pop */
	assert(state->stack);
	/* destroy old frame */
	stack_destroy(old);
}

void
state_declare(struct state *state, struct ast_variable *var, bool isparam)
{
	stack_declare(state->stack, var, isparam);
}

struct ast_variable **
state_getvariables(struct state *state)
{
	return stack_getvariables(state->stack);
}

int
state_nvariables(struct state *state)
{
	return stack_nvariables(state->stack);
}

struct location *
state_getresultloc(struct state *state)
{
	struct variable *v = stack_getresult(state->stack);
	return variable_location(v);
}

static struct location *
location_from_access(struct ast_expr *acc, struct state *state);

struct location *
state_location_from_lvalue(struct state *state, struct ast_expr *lval)
{
	switch (ast_expr_kind(lval)) {
	case EXPR_IDENTIFIER:
		return stack_getlocation(state->stack, ast_expr_as_identifier(lval));
	case EXPR_ACCESS:
		return location_from_access(lval, state);
	default:
		assert(false);
	}	
}

static struct object *
object_from_lvalue(struct state *, struct ast_expr *lvalue);

static struct location *
location_from_access(struct ast_expr *acc, struct state *state)
{
	/* from access `arr[offset]` get `arr` */
	struct object *arr = object_from_lvalue(state, ast_expr_access_root(acc));
	if (!arr) { /* `arr` freed */
		return NULL;
	}

	/* deref to obtain `*arr` */
	struct value *arr_val = object_value(arr);
	if (!arr_val) { /* `arr` uninitialized */
		return NULL;
	}
	struct location *deref = value_as_ptr(arr_val);
	assert(deref);

	/* return lvalue `*(arr+offset)` */
	return location_with_offset(deref, ast_expr_access_index(acc));
}

static struct object *
object_from_lvalue(struct state *state, struct ast_expr *lvalue)
{
	struct location *loc = state_location_from_lvalue(state, lvalue);
	assert(loc);
	struct object *obj = location_getobject(loc, state->stack, state->heap);
	location_destroy(loc);
	return obj;
}

struct location *
state_location_from_rvalue(struct state *state, struct ast_expr *rvalue)
{
	struct object *obj = object_from_lvalue(state, rvalue);
	if (!obj) { /* error: invalid location */
		return NULL;
	}
	struct value *v = object_value(obj);
	assert(v); /* TODO: user error for setting to uninitialised value */
	return location_copy(value_as_ptr(v));
}

struct error *
state_location_assign(struct state *state, struct location *l, struct location *r)
{
	struct object *l_obj = location_getobject(l, state->stack, state->heap);
	if (!l_obj) {
		return error_create("location not found");
	}
	object_assign(l_obj, value_ptr_create(r));
	return NULL;
}

struct error *
state_location_range_alloc(struct state *state, struct location *loc,
		struct ast_expr *lw, struct ast_expr *up)
{
	/* loc corresponds to, say, `arr`, so we dereference */
	struct object *arr = location_getobject(loc, state->stack, state->heap);
	if (!arr) {
		return error_create("no object");
	}
	struct value *arr_val = object_value(arr);
	if (!arr_val) {
		return error_create("no value");
	}
	struct location *deref = value_as_ptr(arr_val);

	block *b = location_getblock(deref, state->stack, state->heap);
	if (!b) {
		return error_create("no block");
	}

	/* TODO: prevent creation of virtual block for empty ranges */ 
	assert(!ast_expr_equal(lw, up));

	/* virtual block to represents range of values allocated */
	return block_range_alloc(b, lw, up, state->heap);
}

struct error *
state_location_dealloc(struct state *state, struct location *loc)
{
	return location_dealloc(loc, state->heap);
}

struct error *
state_location_range_dealloc(struct state *state, struct location *loc,
		struct ast_expr *lw, struct ast_expr *up)
{
	/* loc corresponds to, say, `arr`, so we dereference */
	struct object *arr = location_getobject(loc, state->stack, state->heap);
	if (!arr) {
		return error_create("no object");
	}
	struct value *arr_val = object_value(arr);
	if (!arr_val) {
		return error_create("no value");
	}
	struct location *deref = value_as_ptr(arr_val);
	return location_range_dealloc(deref, lw, up, state->stack, state->heap);
}

bool
state_location_addresses_deallocand(struct state *state, struct location *loc)
{
	struct object *obj = location_getobject(loc, state->stack, state->heap);
	struct value *val = object_value(obj);
	struct location *val_loc = value_location(val); 
	
	return location_isdeallocand(val_loc, state->heap);
}

bool
state_location_range_aredeallocands(struct state *state, struct location *loc,
		struct ast_expr *lw, struct ast_expr *up)
{
	block *b = location_getblock(loc, state->stack, state->heap);
	return (bool) b && block_range_aredeallocands(b, lw, up, state->heap);
}

struct location *
state_alloc(struct state *state, int size)
{
	return heap_newblock(state->heap, size);
}

bool
state_abstractly_equivalent(struct state *actual, struct state *alleged)
{
	struct stack *s1 = actual->stack,
		     *s2 = alleged->stack;
	struct heap *h1 = actual->heap,
		    *h2 = alleged->heap;

	struct variable *r1 = stack_getresult(s1),
			*r2 = stack_getresult(s2);

	if (!variable_heap_equivalent(r1, r2, s1, s2, h1, h2)) {
		return false;
	}
	struct map *m1 = stack_getvarmap(s1),
		   *m2 = stack_getvarmap(s2);
	for (int i = 0; i < m1->n; i++) {
		struct entry e1 = m1->entry[i];
		struct variable *v1 = (struct variable *) e1.value;

		if (variable_isparam(v1)) {
			struct variable *v2 = (struct variable *) map_get(m2, e1.key);
			if (!v2) {
				/* ERROR: doesn't exist */
				return false;
			}
			if (!variable_heap_equivalent(v1, v2, s1, s2, h1, h2)) {
				/* ERROR: variables not heap equiv */
				return false;
			}
		}
	}
	return true;
}

bool
state_heap_referenced(struct state *state)
{
	return heap_referenced(state->heap, state->stack);
}

void
state_location_destroy(struct location *loc)
{
	return location_destroy(loc);
}
