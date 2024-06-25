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
	char *context = externals_types_str(state->ext, "\t");
	if (strlen(context) > 0) {
		strbuilder_printf(b, "context:\n%s\n", context);
	}
	free(context);
	char *static_mem = static_memory_str(state->static_memory, "\t");
	if (strlen(static_mem) > 0) {
		strbuilder_printf(b, "static:\n%s\n", static_mem);
	}
	free(static_mem);
	if (state->reg) {
		char *ret = value_str(state->reg);
		strbuilder_printf(b, "return:\t<%s>\n\n", ret);
		free(ret);
	}
	char *vconst = vconst_str(state->vconst, "\t");
	if (strlen(vconst) > 0) {
		strbuilder_printf(b, "rconst:\n%s\n", vconst);
	}
	free(vconst);
	char *props = props_str(state->props, "\t");
	if (strlen(props) > 0) {
		strbuilder_printf(b, "assume:\n%s\n", props);
	}
	free(props);
	char *clump = clump_str(state->clump, "\t");
	if (strlen(clump) > 0) {
		strbuilder_printf(b, "clump:\n%s\n", clump);
	}
	free(clump);
	char *stack = stack_str(state->stack, state);
	if (strlen(stack) > 0) {
		strbuilder_printf(b, "stack:\n%s\n", stack);
	}
	free(stack);	
	char *heap = heap_str(state->heap, "\t");
	if (strlen(heap) > 0) {
		strbuilder_printf(b, "heap:\n%s\n", heap);
	}
	free(heap);
	return strbuilder_build(b);
}

bool
state_islinear(struct state *s)
{
	return stack_islinear(s->stack);
}

bool
state_insetup(struct state *s)
{
	return stack_insetup(s->stack);	
}

char *
state_execmode_str(enum execution_mode m)
{
	char *execmode_type_str[] = {
		[EXEC_ABSTRACT] 		= "ABSTRACT",
		[EXEC_ABSTRACT_NO_SETUP] 	= "INTERNAL",
		[EXEC_SETUP]			= "INTERNAL",
		[EXEC_ACTUAL]			= "ACTUAL",
		[EXEC_VERIFY]			= "VERIFY",
	};
	switch (m) {
	case EXEC_ABSTRACT:
	case EXEC_ABSTRACT_NO_SETUP:
	case EXEC_SETUP:
	case EXEC_ACTUAL:
	case EXEC_VERIFY:
		return dynamic_str(execmode_type_str[m]);
	default:
		assert(false);
	}
}

enum execution_mode
state_execmode(struct state *s)
{
	return stack_execmode(s->stack);
}

struct lexememarker *
state_lexememarker(struct state *s)
{
	return stack_lexememarker(s->stack);
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

struct error *
state_next(struct state *s)
{
	return stack_next(s->stack, s);
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
	stack_storeloc(state->stack);
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

void
state_initsetup(struct state *s, int frameid)
{
	struct error *err;
	
	assert(state_execmode(s) == EXEC_SETUP);
	while (!(state_atend(s) && state_frameid(s) == frameid)) {
		if ((err = state_step(s))) {
			assert(false);
		}
	}
}

enum execution_mode
state_next_execmode(struct state *s)
{
	if (stack_execmode(s->stack) == EXEC_ABSTRACT) {
		return EXEC_ABSTRACT;
	}
	return EXEC_ABSTRACT_NO_SETUP;
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

static struct location *
allocstatic(char *lit, struct state *);

struct value *
state_static_init(struct state *state, struct ast_expr *expr)
{
	char *lit = ast_expr_as_literal(expr);
	struct location *loc = allocstatic(lit, state);
	return value_ptr_create(loc);
}

static struct location *
allocstatic(char *lit, struct state *state)
{
	struct location *loc = static_memory_checkpool(state->static_memory, lit);
	if (loc) {
		return loc;
	}

	int address = static_memory_newblock(state->static_memory);
	loc = location_create_static(
		address,
		offset_create(ast_expr_constant_create(0))
	);
	struct object_res *res = state_get(state, loc, true);
	assert(object_res_hasobject(res));
	object_assign(
		object_res_as_object(res),
		value_literal_create(dynamic_str(lit))
	);
	static_memory_stringpool(state->static_memory, lit, loc);
	return loc;
}

struct value *
state_clump(struct state *state)
{
	/* XXX: should type be associated with blocks for type checking when we
	 * assign? */
	int address = clump_newblock(state->clump);
	struct location *loc = location_create_dereferencable(
		address,
		offset_create(ast_expr_constant_create(0))
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
	struct object_res *res = state_get(state, loc, true); /* put object there */
	if (object_res_iserror(res)) {
		struct error *err = object_res_as_error(res);
		assert(
			error_to_block_observe_noobj(err)
			|| error_to_state_get_no_block(err)
		);
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
	struct object_res *res = state_get(state, loc, true); /* put object there */
	if (object_res_iserror(res)) {
		assert(false);
	}
	return location_toheap(loc, state->heap);
}

struct value *
state_getvconst(struct state *state, char *id)
{
	return vconst_get(state->vconst, id);
}

struct object_res *
state_get(struct state *state, struct location *loc, bool constructive)
{
	struct block_res *res = location_getblock(
		loc,
		state->static_memory,
		state->vconst,
		state->stack,
		state->heap,
		state->clump
	);
	if (block_res_iserror(res)) {
		return object_res_error_create(block_res_as_error(res));
	}
	if (!block_res_hasblock(res)) {
		switch (location_type(loc)) {
		case LOCATION_DYNAMIC:
		case LOCATION_DEREFERENCABLE:
		case LOCATION_STATIC:
			return object_res_error_create(
				error_state_get_no_block()
			);
		default:
			assert(false);
		}
	}

	struct offset *of = location_offset(loc);
	struct ast_expr *base_offset = offset_offset(of);
	struct object_res *obj_res = block_observe(
		block_res_as_block(res),
		base_offset, state, constructive
	);
	char *structmember = offset_member(of);
	if (!structmember) {
		return obj_res;
	}

	struct ast_type *t = offset_membertype(of);
	struct object *root_obj = object_res_as_object(obj_res);
	struct object *member = object_getmember(root_obj, t, structmember, state
	);
	if (!member) {
		char *type_str = ast_type_str(t);
		struct error *e = error_printf(
			"`%s' has no member `%s'", type_str, structmember
		);
		free(type_str);
		return object_res_error_create(e);
	}
	return object_res_object_create(member);
}

void
state_blockinstall(struct block *b, struct object *obj)
{
	block_install(b, obj);
}

struct block *
state_getblock(struct state *state, struct location *loc)
{
	struct block_res *res = location_getblock(
		loc, state->static_memory, state->vconst, state->stack, state->heap, state->clump
	);
	return block_res_as_block(res);
}

struct ast_type *
state_getvariabletype(struct state *state, char *id)
{
	if (strcmp(id, KEYWORD_RETURN) == 0) {
		assert(false);
		//return state_getresulttype(state);
	}

	struct variable *v = stack_getvariable(state->stack, id);
	assert(v);

	return variable_type(v);
}

bool
state_isparam(struct state *state, char *id)
{
	struct variable *v = stack_getvariable(state->stack, id);
	assert(v);
	return variable_isparam(v);
}

struct location *
state_getloc(struct state *state, char *id)
{
	struct variable *v = stack_getvariable(state->stack, id);
	assert(v);
	return variable_location(v);
}

struct object_res *
state_getobject(struct state *state, char *id)
{
	if (strcmp(id, KEYWORD_RETURN) == 0) {
		assert(false);
		/*return state_getresult(state);*/
	}

	struct variable *v = stack_getvariable(state->stack, id);
	if (!v) {
		return object_res_error_create(
			error_printf("unknown variable `%s'", id)
		);
	}

	return state_get(state, variable_location(v), true);
}

struct object_res *
state_deref(struct state *state, struct value *ptr_val)
{
	if (value_issync(ptr_val)) {
		return object_res_error_create(error_state_deref_rconst());
	}
	struct location *deref = value_as_location(ptr_val);
	assert(deref);

	/* `*(ptr+offset)` */
	struct object_res *res = state_get(state, deref, true);
	if (object_res_iserror(res)) {
		struct error *err = object_res_as_error(res);
		if (!error_to_state_get_no_block(err)) {
			return object_res_error_create(
				error_printf(
					"undefined indirection: %s",
					error_str(err)
				)
			);
		}
		object_res_errorignore(res);
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

	struct block_res *res = location_getblock(
		deref, state->static_memory, state->vconst, state->stack, state->heap, state->clump
	);
	if (!block_res_hasblock(res)) {
		return error_printf("no block");
	}

	/* TODO: prevent creation of virtual block for empty ranges */ 
	assert(!ast_expr_equal(lw, up));

	/* virtual block to represents range of values allocated */
	return block_range_alloc(block_res_as_block(res), lw, up, state->heap);
}

struct location *
state_alloc(struct state *state, int size)
{
	if (state_insetup(state)) {
		return heap_newcallerblock(state->heap, size);
	}
	return heap_newblock(state->heap, size);
}

struct error *
state_dealloc(struct state *state, struct location *loc)
{
	return location_dealloc(loc, state->heap);
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
	
	struct block_res *res = location_getblock(
		deref, state->static_memory, state->vconst, state->stack, state->heap, state->clump
	);
	struct block *b = block_res_as_block(res);
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
state_returnreferences(struct state *s, struct location *loc)
{
	struct circuitbreaker *c = circuitbreaker_create();
	bool refs = s->reg && value_references(s->reg, loc, s, c);
	circuitbreaker_destroy(c);
	return refs;
}

bool
state_callerreferences(struct state *s, struct location *loc)
{
	return clump_callerreferences(s->clump, loc, s);
}

bool
state_eval(struct state *s, struct ast_expr *e)
{
	return vconst_eval(s->vconst, e);
}

static void
state_normalise(struct state *s);

bool
state_equal(struct state *s1, struct state *s2)
{
	struct state *s1_c = state_copy(s1),
		     *s2_c = state_copy(s2);
	state_normalise(s1_c);
	state_normalise(s2_c);

	char *str1 = state_str(s1_c),
	     *str2 = state_str(s2_c);
	bool equal = strcmp(str1, str2) == 0;
	if (!equal) {
		v_printf("actual:\n%s", str1);
		v_printf("abstract:\n%s", str2);
	}
	free(str2);
	free(str1);

	state_destroy(s2_c);
	state_destroy(s1_c);

	return equal;
}

static void
state_undeclareliterals(struct state *s);

static void
state_undeclarevars(struct state *s);

static void
state_popprops(struct state *s);

void
state_unnest(struct state *s);

static void
state_permuteheap(struct state *, struct int_arr *);

static struct int_arr *
deriveorder(struct state *s);

static void
state_normalise(struct state *s)
{
	state_unnest(s);
	state_undeclareliterals(s);
	state_undeclarevars(s);
	state_popprops(s);
	if (s->reg) {
		state_permuteheap(s, deriveorder(s));
	}
}

static struct int_arr *
deriveorder(struct state *s)
{
	struct circuitbreaker *cb = circuitbreaker_create();
	struct int_arr *arr = value_deriveorder(s->reg, cb, s);
	circuitbreaker_destroy(cb);
	heap_fillorder(s->heap, arr);
	return arr;
}

static void
state_permuteheap(struct state *s, struct int_arr *arr)
{
	/* XXX: only permuting heap and return register; clump later */
	assert(s->reg);

	struct permutation *p = permutation_create(arr);
	s->heap = heap_permute(s->heap, p);
	s->reg = value_permuteheaplocs(s->reg, p);
	permutation_destroy(p);
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
