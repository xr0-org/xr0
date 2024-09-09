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
#include "stack.h"
#include "state.h"
#include "static.h"
#include "util.h"
#include "value.h"
#include "verifier.h"

struct state {
	struct externals *ext;
	struct rconst *rconst;
	struct static_memory *static_memory;
	struct clump *clump;
	struct stack *stack;
	struct heap *heap;
	struct value *reg;
};

struct state *
state_create(struct frame *f, struct rconst *rconst, struct externals *ext)
{
	struct state *state = malloc(sizeof(struct state));
	assert(state);
	state->ext = ext;
	state->static_memory = static_memory_create();
	state->rconst = rconst;
	state->clump = clump_create();
	state->stack = stack_create(f, NULL);
	state->heap = heap_create();
	state->reg = NULL;
	return state;
}

void
state_destroy(struct state *state)
{
	/* rconst_destroy(state->rconst); */
	static_memory_destroy(state->static_memory);
	clump_destroy(state->clump);
	stack_destroy(state->stack);
	heap_destroy(state->heap);
	free(state);
}

struct state *
state_copy(struct state *state)
{
	struct state *copy = malloc(sizeof(struct state));
	assert(copy);
	copy->ext = state->ext;
	copy->static_memory = static_memory_copy(state->static_memory);
	copy->rconst = rconst_copy(state->rconst);
	copy->clump = clump_copy(state->clump);
	copy->stack = stack_copy(state->stack);
	copy->heap = heap_copy(state->heap);
	copy->reg = state->reg ? value_copy(state->reg) : NULL;
	return copy;
}

struct state *
state_split(struct state *state, struct rconst *rconst, char *func_name)
{
	struct state *copy = state_copy(state);
	copy->rconst = rconst;
	copy->stack = stack_copywithname(state->stack, func_name);
	return copy;
}

char *
state_funcname(struct state *s)
{
	return stack_funcname(s->stack);
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
	char *rconst = rconst_str(state->rconst, "\t");
	if (strlen(rconst) > 0) {
		strbuilder_printf(b, "rconst:\n%s\n", rconst);
	}
	free(rconst);
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

struct stack *
state_stack(struct state *s)
{
	return s->stack;
}

int
state_modecanverify(struct state *s)
{
	return stack_modecanverify(s->stack);
}

int
state_modecanrunxr0cmd(struct state *s)
{
	return stack_modecanrunxr0cmd(s->stack);
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

bool
state_atsetupend(struct state *s)
{
	return stack_atsetupend(s->stack);
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
state_rconst(struct state *state, struct ast_type *t, struct ast_expr *range,
		char *key, bool persist)
{
	assert(key);

	char *prev = rconst_getidbykey(state->rconst, key);
	if (prev) {
		return value_rconst_create(
			ast_expr_identifier_create(dynamic_str(prev))
		);
	}
	struct value *v = ast_type_rconst(t, state, range, key, persist);
	if (value_isstruct(v)) {
		return v;
	}
	char *c = rconst_declare(state->rconst, v, key, persist);
	return value_rconst_create(ast_expr_identifier_create(c));
}

struct value *
state_rconstnokey(struct state *state, struct ast_type *t, struct ast_expr *range,
		bool persist)
{
	struct value *v = ast_type_rconstnokey(t, state, range, persist);
	if (value_isstruct(v)) {
		return v;
	}
	char *c = rconst_declarenokey(state->rconst, v, persist);
	return value_rconst_create(ast_expr_identifier_create(c));
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

char *
state_argmodulator(struct state *s)
{
	return stack_argmodulator(s->stack, s);
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
	return value_ptr_create(
		location_create_dereferencable(
			clump_newblock(state->clump),
			offset_create(ast_expr_constant_create(0))
		)
	);
}

bool
state_islval(struct state *state, struct location *loc)
{
	return location_tostatic(loc, state->static_memory)
		|| location_toheap(loc, state->heap)
		|| location_tostack(loc, state->stack)
		|| location_toclump(loc, state->clump);
}

bool
state_isalloc(struct state *state, struct location *loc)
{
	struct object_res *res = state_get(state, loc, true); /* put object there */
	if (object_res_iserror(res)) {
		assert(false);
	}
	return location_toheap(loc, state->heap);
}

struct value *
state_getrconst(struct state *state, char *id)
{
	return rconst_get(state->rconst, id);
}

struct object_res *
state_get(struct state *state, struct location *loc, bool constructive)
{
	struct block_res *res = location_getblock(
		loc,
		state->static_memory,
		state->rconst,
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

struct block *
state_getblock(struct state *state, struct location *loc)
{
	struct block_res *res = location_getblock(
		loc,
		state->static_memory,
		state->rconst,
		state->stack,
		state->heap,
		state->clump
	);
	if (block_res_hasblock(res)) {
		return block_res_as_block(res);
	}
	return NULL;
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

struct loc_res *
state_getloc(struct state *state, char *id)
{
	struct variable *v = stack_getvariable(state->stack, id);
	if (!v) {
		return loc_res_error_create(
			error_printf("unknown variable `%s'", id)
		);
	}
	return loc_res_loc_create(variable_location(v));
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


struct location *
state_alloc(struct state *state, int size)
{
	if (stack_insetup(state->stack)) {
		return heap_newcallerblock(state->heap, size);
	}
	return heap_newblock(state->heap, size);
}

struct error *
state_dealloc(struct state *state, struct location *loc)
{
	return location_dealloc(loc, state->heap);
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
	return rconst_eval(s->rconst, e);
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
	rconst_undeclare(s->rconst);
	struct value *v = state_readregister(s);
	if (v) {
		/* XXX: avoid state_writeregister assert */
		s->reg = value_abstractcopy(v, s);
	}
	stack_undeclare(s->stack, s);
}

void
state_unnest(struct state *s)
{
	while (stack_isnested(s->stack)) {
		state_popframe(s);
	}
}
