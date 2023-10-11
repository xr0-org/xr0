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

struct externals {
	struct map *func, *type, *var;
};

struct externals *
externals_create()
{
	struct externals *ext = malloc(sizeof(struct externals));
	ext->func = map_create();
	ext->var = map_create();
	ext->type = map_create();
	return ext;
}

void
externals_destroy(struct externals *ext)
{
	map_destroy(ext->func);
	map_destroy(ext->var);
	map_destroy(ext->type);
	free(ext);
}

static char *
externals_types_str(struct externals *ext, char *indent)
{
	struct strbuilder *b = strbuilder_create();

	struct map *m = ext->type;
	for (int i = 0; i < m->n; i++) {
		char *type = ast_type_str((struct ast_type *) m->entry[i].value);
		strbuilder_printf(b, "%s%s\n", indent, type);
		free(type);
	}

	return strbuilder_build(b);
}

void
externals_declarefunc(struct externals *ext, char *id, struct ast_function *f)
{
	map_set(ext->func, dynamic_str(id), f);
}

void
externals_declarevar(struct externals *ext, char *id, struct ast_variable *v)
{
	map_set(ext->var, dynamic_str(id), v);
}

void
externals_declaretype(struct externals *ext, char *id, struct ast_type *t)
{
	map_set(ext->type, dynamic_str(id), t);
}

struct ast_function *
externals_getfunc(struct externals *ext, char *id)
{
	return map_get(ext->func, id);
}

struct ast_type *
externals_gettype(struct externals *ext, char *id)
{
	return map_get(ext->type, id);
}



struct state {
	struct externals *ext;
	struct vconst *vconst;
	struct stack *stack;
	struct heap *heap;
};

struct state *
state_create(struct externals *ext, struct ast_type *result_type)
{
	struct state *state = malloc(sizeof(struct state));
	assert(state);
	state->ext = ext;
	state->vconst = vconst_create();
	state->stack = stack_create(NULL, result_type);
	state->heap = heap_create();
	return state;
}

void
state_destroy(struct state *state)
{
	/* vconst_destroy(state->vconst); */
	stack_destroy(state->stack);
	heap_destroy(state->heap);
	free(state);
}

struct state *
state_copy(struct state *state)
{
	struct state *copy = malloc(sizeof(struct state));
	assert(state);
	copy->ext = state->ext;
	copy->vconst = vconst_copy(state->vconst);
	copy->stack = stack_copy(state->stack);
	copy->heap = heap_copy(state->heap);
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

struct ast_function *
state_getfunc(struct state *state, char *f)
{
	return externals_getfunc(state->ext, f);
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

void
state_stack_undeclare(struct state *s)
{
	stack_undeclare(s->stack);
}

struct value *
state_vconst(struct state *state)
{
	char *c = vconst_declare(state->vconst, value_int_any_create());
	return value_int_sync_create(ast_expr_identifier_create(c));
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
state_gettype(struct state *state, char *id)
{
	if (strcmp(id, KEYWORD_RESULT) == 0) {
		return state_getresulttype(state);
	}

	struct variable *v = stack_getvariable(state->stack, id);
	assert(v);

	return variable_type(v);
}

struct object *
state_getobject(struct state *state, char *id)
{
	if (strcmp(id, KEYWORD_RESULT) == 0) {
		return state_getresult(state);
	}

	struct variable *v = stack_getvariable(state->stack, id);
	assert(v);

	return state_get(state, variable_location(v), true);
}

struct value *
getorcreatestruct(struct object *, struct ast_type *, struct state *);

struct object *
state_getobjectmember(struct state *state, struct object *obj,
		struct ast_type *t, char *member)
{
	return value_struct_member(
		getorcreatestruct(obj, t, state), member
	);
}

struct ast_type *
usecomplete(struct ast_type *, struct state *);

struct value *
getorcreatestruct(struct object *obj, struct ast_type *t, struct state *state)
{
	struct value *v = object_as_value(obj);
	if (v) {
		return v;
	}
	struct ast_type *complete = usecomplete(t, state);
	assert(complete);
	v = value_struct_create(complete);
	object_assign(obj, v);
	return v;
}

struct ast_type *
usecomplete(struct ast_type *t, struct state *state)
{
	if (ast_type_struct_members(t)) {
		return t;
	}
	char *tag = ast_type_struct_tag(t);
	assert(tag);
	return externals_gettype(state->ext, tag);
}

struct ast_type *
state_getobjectmembertype(struct state *state, struct object *obj,
		struct ast_type *t, char *member)
{
	return value_struct_membertype(
		getorcreatestruct(obj, t, state), member
	);
}

struct ast_expr *
resolve_bound(struct ast_expr *, struct state *);

struct object *
state_deref(struct state *state, struct object *ptr, struct ast_expr *index)
{
	struct value *ptr_val = object_as_value(ptr);
	if (!ptr_val) { /* `ptr` uninitialized */
		return NULL;
	}
	struct location *deref_base = value_as_ptr(ptr_val);
	assert(deref_base);

	/* `*(ptr+offset)` */
	struct ast_expr *res_index = resolve_bound(index, state);
	struct location *deref = location_with_offset(deref_base, res_index);
	/*ast_expr_destroy(res_index);*/
	struct object *res = state_get(state, deref, true);
	location_destroy(deref);
	return res;
}

struct value *
value_from_access(struct ast_expr *acc, struct state *state);

struct value *
state_getvalue(struct state *state, struct ast_expr *rvalue)
{
	switch (ast_expr_kind(rvalue)) {
	case EXPR_ACCESS:
		return value_from_access(rvalue, state);
	case EXPR_IDENTIFIER:
		break;
	default:
		assert(false);
	}

	/* XXX: side effect of observing if necessary */
	resolve_bound(rvalue, state);

	struct object *obj = state_getobject(
		state, ast_expr_as_identifier(rvalue)
	);
	if (!obj) { /* error: unknown identifier */
		return NULL;
	}
	struct value *v = object_as_value(obj);

	/* XXX: what to do in cases where this is a parameter to a function */
	assert(v); /* TODO: user error for setting to uninitialised value */
	return value_copy(v);
}

struct value *
value_from_access(struct ast_expr *acc, struct state *state)
{
	struct value *arr = state_getvalue(state, ast_expr_access_root(acc));
	assert(arr);
	struct location *loc = value_as_ptr(arr);
	assert(loc);
	struct location *deref = location_with_offset(
		loc, resolve_bound(ast_expr_access_index(acc), state)
	);
	value_destroy(arr);
	struct object *obj = state_get(state, deref, true);
	struct value *v = object_as_value(obj);
	assert(v);
	return value_copy(v);
}

struct error *
state_assign(struct state *state, struct object *obj, struct value *val)
{
	object_assign(obj, val);
	return NULL;
}

struct value *
state_boundedincrement(struct state *s, char *id)
{
	struct value *sync = state_getvalue(s, ast_expr_identifier_create(id));
	char *oldc = ast_expr_as_identifier(value_as_sync(sync));
	struct value *old = vconst_get(s->vconst, oldc);
	assert(old);

	struct value *new = value_int_range_create(
		value_int_lw(old)+1, value_int_up(old)
	);

	char *c = vconst_declare(s->vconst, new);
	return value_int_sync_create(ast_expr_identifier_create(c));
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
	struct location *deref = value_as_ptr(arr_val);

	struct block *b = location_getblock(
		deref, state->vconst, state->stack, state->heap
	);
	if (!b) {
		return error_create("no block");
	}

	/* TODO: prevent creation of virtual block for empty ranges */ 
	assert(!ast_expr_equal(lw, up));

	struct ast_expr *res_lw = resolve_bound(lw, state),
			*res_up = resolve_bound(up, state);

	/* virtual block to represents range of values allocated */
	return block_range_alloc(b, res_lw, res_up, state->heap);
}

static struct ast_expr *
resolve_bound_identifier(char *, struct state *);

static struct ast_expr *
resolve_structmember(struct ast_expr *, struct state *);

struct ast_expr *
resolve_bound(struct ast_expr *bound, struct state *state)
{
	/* TODO: incorporate recursive logic for binary ops */
	switch (ast_expr_kind(bound)) {
	case EXPR_CONSTANT:
		return bound;
	case EXPR_IDENTIFIER:
		return resolve_bound_identifier(
			ast_expr_as_identifier(bound), state
		);
	case EXPR_STRUCTMEMBER:
		return resolve_structmember(bound, state);
	default:
		assert(false);
	}
}

static struct ast_expr *
resolve_bound_identifier(char *id, struct state *state)
{
	/* XXX */
	if (id[0] == '$') {
		return ast_expr_identifier_create(dynamic_str(id));
	}

	struct variable *var = strcmp(id, KEYWORD_RESULT) == 0
		? stack_getresult(state->stack)
		: stack_getvariable(state->stack, id);
	assert(var);

	/* XXX: all observable types */
	if (!variable_isobservable(var)) { 
		return ast_expr_identifier_create(dynamic_str(id));
	}

	struct object *obj = state_getobject(state, id);
	assert(obj);

	struct value *val = value_copy(object_as_value(obj));
	if (!val) {
		return NULL;
	} else if (value_issync(val)) {
		return value_as_sync(val);
	} else if (value_isconstant(val)) {
		return ast_expr_constant_create(value_as_constant(val));
	}

	char *c = vconst_declare(state->vconst, val);
	object_assign(obj, value_int_sync_create(ast_expr_identifier_create(c)));

	return ast_expr_identifier_create(dynamic_str(c));
}

static struct ast_expr *
resolve_structmember(struct ast_expr *expr, struct state *s)
{
	assert(false);
}


struct value *
state_alloc(struct state *state)
{
	return value_ptr_create(heap_newblock(state->heap));
}

struct error *
state_dealloc(struct state *state, struct value *val)
{
	return location_dealloc(value_as_ptr(val), state->heap);
}

struct value *
state_getvconst(struct state *state, char *id)
{
	return vconst_get(state->vconst, id);
}


struct error *
state_range_dealloc(struct state *state, struct object *obj,
		struct ast_expr *lw, struct ast_expr *up)
{
	struct ast_expr *res_lw = resolve_bound(lw, state),
			*res_up = resolve_bound(up, state);

	/* obj corresponds to, say, `arr`, so we dereference */
	struct value *arr_val = object_as_value(obj);
	if (!arr_val) {
		return error_create("no value");
	}
	struct location *deref = value_as_ptr(arr_val);
	return location_range_dealloc(deref, res_lw, res_up, state);
}

bool
state_addresses_deallocand(struct state *state, struct object *obj)
{
	struct value *val = object_as_value(obj);
	struct location *loc = value_as_ptr(val); 
	
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
	struct location *deref = value_as_ptr(arr_val);
	
	struct block *b = location_getblock(
		deref, state->vconst, state->stack, state->heap
	);
	return (bool) b && block_range_aredeallocands(b, lw, up, state);
}

bool
state_heap_referenced(struct state *state)
{
	return heap_referenced(state->heap, state);
}

void
state_location_destroy(struct location *loc)
{
	location_destroy(loc);
}

void
state_value_destroy(struct value *val)
{
	assert(val);
	value_destroy(val);
}

struct value *
state_value_copy(struct value *v)
{
	return value_copy(v);
}

void
state_object_destroy(struct object *obj)
{
	/* XXX */
}

bool
state_stack_references(struct state *s, struct location *loc)
{
	return stack_references(s->stack, loc, s);
}

bool
state_eval(struct state *s, struct ast_expr *e)
{
	return vconst_eval(s->vconst, e);
}

bool
state_equal(struct state *s1, struct state *s2)
{
	bool equal;
	char *str1 = state_str(s1),
	     *str2 = state_str(s2);
	equal = strcmp(str1, str2) == 0;

	free(str2);
	free(str1);

	return equal;
}
