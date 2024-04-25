#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "block.h"
#include "clump.h"
#include "ext.h"
#include "heap.h"
#include "static.h"
#include "location.h"
#include "object.h"
#include "props.h"
#include "stack.h"
#include "state.h"
#include "static.h"
#include "util.h"
#include "value.h"

struct state {
	struct externals *ext;
	struct vconst *vconst;
	struct static_memory *static_memory;
	struct clump *clump;
	struct stack *stack;
	struct heap *heap;
	struct props *props;
	struct value *reg;
};

struct state *
state_create(struct frame *f, struct externals *ext)
{
	struct state *state = malloc(sizeof(struct state));
	assert(state);
	state->ext = ext;
	state->static_memory = static_memory_create();
	state->vconst = vconst_create();
	state->clump = clump_create();
	state->stack = stack_create(f, NULL);
	state->heap = heap_create();
	state->props = props_create();
	state->reg = NULL;
	return state;
}

struct state *
state_create_withprops(struct frame *f, struct externals *ext,
		struct props *props)
{
	struct state *state = state_create(f, ext);
	state->props = props_copy(props);
	return state;
}

void
state_destroy(struct state *state)
{
	/* vconst_destroy(state->vconst); */
	static_memory_destroy(state->static_memory);
	clump_destroy(state->clump);
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
	copy->static_memory = static_memory_copy(state->static_memory);
	copy->vconst = vconst_copy(state->vconst);
	copy->clump = clump_copy(state->clump);
	copy->stack = stack_copy(state->stack);
	copy->heap = heap_copy(state->heap);
	copy->props = props_copy(state->props);
	copy->reg = state->reg ? value_copy(state->reg) : NULL;
	return copy;
}

struct state *
state_copywithname(struct state *state, char *func_name)
{
	struct state *copy = state_copy(state);
	copy->stack = stack_copywithname(state->stack, func_name);
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
	strbuilder_printf(
		b,
		"\treturn: {<type> := {%s}}\n\n",
		state->reg ? value_str(state->reg) : "empty"
	);
	char *static_mem = static_memory_str(state->static_memory, "\t");
	if (strlen(static_mem) > 0) {
		strbuilder_printf(b, "%s\n", static_mem);
	}
	free(static_mem);
	char *vconst = vconst_str(state->vconst, "\t");
	if (strlen(vconst) > 0) {
		strbuilder_printf(b, "%s\n", vconst);
	}
	free(vconst);
	char *clump = clump_str(state->clump, "\t");
	if (strlen(clump) > 0) {
		strbuilder_printf(b, "%s\n", clump);
	}
	free(clump);
	char *stack = stack_str(state->stack, state);
	if (strlen(stack) > 0) {
		strbuilder_printf(b, "%s\n", stack);
	}
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

bool
state_islinear(struct state *s)
{
	return stack_islinear(s->stack);
}

bool
state_atend(struct state *s)
{
	return stack_atend(s->stack);
}

struct error *
state_step(struct state *s)
{
	return stack_step(s->stack, s);
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

char *
state_programtext(struct state *s)
{
	return stack_programtext(s->stack);
}

int
state_programindex(struct state *s)
{
	return stack_programindex(s->stack);
}

int
state_frameid(struct state *s)
{
	return stack_id(s->stack);
}

void
state_pushframe(struct state *state, struct frame *f)
{
	state->stack = stack_create(f, state->stack);
}

void
state_popframe(struct state *state)
{
	stack_popprep(state->stack, state);

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
state_popregister(struct state *state)
{
	//v_printf("reading from register: %s\n", state->reg ? value_str(state->reg) : "NULL");
	if (!state->reg) {
		return NULL;
	}
	struct value *v = value_copy(state->reg);
	if (state_frameid(state) != 0) {
		state->reg = NULL;
	}
	return v;
}

struct value *
state_readregister(struct state *state)
{
	//v_printf("reading from register: %s\n", state->reg ? value_str(state->reg) : "NULL");
	return state->reg;
}

void
state_writeregister(struct state *state, struct value *v)
{
	printf("state: %s\n", state_str(state));
	assert(!state->reg);
	//v_printf("writing to register: %s\n", v ? value_str(v) : "NULL");
	state->reg = v;
}

void
state_clearregister(struct state *state)
{
	/* XXX: used after initing the actual state */
	state->reg = NULL;
}

struct error *
state_stacktrace(struct state *s, struct error *err)
{
	return stack_trace(s->stack, err);
}

void
state_return(struct state *s)
{
	stack_return(s->stack);	
}

struct ast_expr *
state_framecall(struct state *s)
{
	return stack_framecall(s->stack);
}

struct value *
state_static_init(struct state *state, struct ast_expr *expr)
{
	char *lit = ast_expr_as_literal(expr);
	struct location *loc = static_memory_checkpool(state->static_memory, lit);
	if (loc) {
		return value_ptr_create(loc);
	}
	int address = static_memory_newblock(state->static_memory);
	loc = location_create_static(
		address,
		ast_expr_constant_create(0)
	);
	struct object_res res = state_get(state, loc, true);
	if (res.err) {
		assert(false);
	}
	if (!res.obj) {
		assert(false);
	}
	object_assign(res.obj, value_literal_create(dynamic_str(lit)));

	static_memory_stringpool(state->static_memory, lit, loc);
	
	return value_ptr_create(loc);
}

struct value *
state_clump(struct state *state)
{
	/* XXX: should type be associated with blocks for type checking when we
	 * assign? */
	int address = clump_newblock(state->clump);
	struct location *loc = location_create_dereferencable(
		address,
		ast_expr_constant_create(0)
	);
	return value_ptr_create(loc);
}

bool
state_islval(struct state *state, struct value *v)
{
	assert(v);
	if (!value_islocation(v)) {
		return false;
	}
	struct location *loc = value_as_location(v);
	struct object_res res= state_get(state, loc, true); /* put object there */
	if (res.err) {
		assert(false);
	}
	return location_tostatic(loc, state->static_memory) ||
		location_toheap(loc, state->heap) ||
		location_tostack(loc, state->stack) ||
		location_toclump(loc, state->clump);
}

bool
state_isalloc(struct state *state, struct value *v)
{
	assert(v);
	if (!value_islocation(v)) {
		return false;
	}
	struct location *loc = value_as_location(v);
	struct object_res res = state_get(state, loc, true); /* put object there */
	if (res.err) {
		assert(false);
	}
	return location_toheap(loc, state->heap);
}

struct value *
state_getvconst(struct state *state, char *id)
{
	return vconst_get(state->vconst, id);
}

struct object_res
state_get(struct state *state, struct location *loc, bool constructive)
{
	struct block_res res = location_getblock(
		loc, state->static_memory, state->vconst, state->stack, state->heap, state->clump
	);
	if (res.err) {
		return (struct object_res) { .obj = NULL, .err = res.err };
	}
	if (!res.b) {
		switch (location_type(loc)) {
		case LOCATION_DYNAMIC:
		case LOCATION_DEREFERENCABLE:
		case LOCATION_STATIC:
			return (struct object_res) { .obj = NULL, .err = NULL };
		default:
			assert(false);
		}
	}
	struct object *obj = block_observe(res.b, location_offset(loc), state, constructive);
	return (struct object_res) { .obj = obj, .err = NULL };
}

void
state_blockinstall(struct block *b, struct object *obj)
{
	block_install(b, obj);
}

struct block *
state_getblock(struct state *state, struct location *loc)
{
	struct block_res res = location_getblock(
		loc, state->static_memory, state->vconst, state->stack, state->heap, state->clump
	);
	if (res.err) {
		assert(false);
	}
	return res.b;
}

struct ast_type *
state_getobjecttype(struct state *state, char *id)
{
	if (strcmp(id, KEYWORD_RETURN) == 0) {
		assert(false);
		//return state_getresulttype(state);
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

struct object_res
state_getobject(struct state *state, char *id)
{
	if (strcmp(id, KEYWORD_RETURN) == 0) {
		assert(false);
		/*return state_getresult(state);*/
	}

	struct variable *v = stack_getvariable(state->stack, id);
	if (!v) {
		return (struct object_res) {
			.err = error_printf("unknown variable `%s'", id)
		};
	}

	return state_get(state, variable_location(v), true);
}

struct object_res
state_deref(struct state *state, struct value *ptr_val, struct ast_expr *index)
{
	if (value_issync(ptr_val)) {
		return (struct object_res) { .obj = NULL, .err = NULL};
	}
	struct location *deref_base = value_as_location(ptr_val);
	assert(deref_base);

	/* `*(ptr+offset)` */
	struct location *deref = location_with_offset(deref_base, index);
	struct object_res res = state_get(state, deref, true);
	if (res.err) {
		return (struct object_res) {
			.obj = NULL,
			.err = error_printf(
				"undefined indirection: %s", error_str(res.err)
			)
		};
	}
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
		return error_printf("no value");
	}

	/* assume pointer */
	struct location *deref = value_as_location(arr_val);

	struct block_res res = location_getblock(
		deref, state->static_memory, state->vconst, state->stack, state->heap, state->clump
	);
	if (res.err) {
		assert(false);
	}
	if (!res.b) {
		return error_printf("no block");
	}

	/* TODO: prevent creation of virtual block for empty ranges */ 
	assert(!ast_expr_equal(lw, up));

	/* virtual block to represents range of values allocated */
	return block_range_alloc(res.b, lw, up, state->heap);
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
		return error_printf("undefined free of value not pointing at heap");
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
		return error_printf("no value");
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
	
	struct block_res res = location_getblock(
		deref, state->static_memory, state->vconst, state->stack, state->heap, state->clump
	);
	if (res.err) {
		assert(false);
	}
	return (bool) res.b && block_range_aredeallocands(res.b, lw, up, state);
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
state_undeclareliterals(struct state *s);

static void
state_undeclarevars(struct state *s);

static void
state_popprops(struct state *s);

void
state_unnest(struct state *s);

bool
state_equal(struct state *s1, struct state *s2)
{
	struct state *s1_c = state_copy(s1),
		     *s2_c = state_copy(s2);
	state_unnest(s1_c);
	state_unnest(s2_c);
	state_undeclareliterals(s1_c);
	state_undeclareliterals(s2_c);
	state_undeclarevars(s1_c);
	state_undeclarevars(s2_c);
	state_popprops(s1_c);
	state_popprops(s2_c);

	char *str1 = state_str(s1_c),
	     *str2 = state_str(s2_c);
	bool equal = strcmp(str1, str2) == 0;
	if (!equal) {
		v_printf("abstract: %s", str2);
		v_printf("actual: %s", str1);
	}
	free(str2);
	free(str1);

	state_destroy(s2_c);
	state_destroy(s1_c);

	return equal;
}

static void
state_undeclareliterals(struct state *s)
{
	static_memory_destroy(s->static_memory);
	/* XXX: map leaks */
	s->static_memory = static_memory_create();
}

static void
state_undeclarevars(struct state *s)
{	
	heap_undeclare(s->heap, s);
	vconst_undeclare(s->vconst);
	struct value *v = state_readregister(s);
	if (v) {
		/* XXX: avoid state_writeregister assert */
		s->reg = value_abstractcopy(v, s);
	}
	stack_undeclare(s->stack, s);
}

static void
state_popprops(struct state *s)
{
	props_destroy(s->props);
	s->props = props_create();
}

void
state_unnest(struct state *s)
{
	while (stack_isnested(s->stack)) {
		state_popframe(s);
	}
}
