#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "ext.h"
#include "object.h"
#include "state.h"
#include "util.h"
#include "lsi.h"
#include "value.h"
#include "verifier.h"

#include "block.h"
#include "clump.h"
#include "constraint.h"
#include "heap.h"
#include "location.h"
#include "stack.h"
#include "static.h"

struct state {
	struct externals *ext;
	struct rconst *rconst;

	/* ram */
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

int
state_atend(struct state *s)
{
	return stack_atend(s->stack);
}

int
state_atsetupend(struct state *s)
{
	return stack_atsetupend(s->stack);
}

int
state_ininvariant(struct state *s)
{
	return stack_ininvariant(s->stack);
}

int
state_atinvariantend(struct state *s)
{
	return stack_atinvariantend(s->stack);
}

int
state_atloopend(struct state *s)
{
	return stack_atloopend(s->stack);
}

void
state_pushinvariantframe(struct state *s, struct ast_block *b)
{
	state_pushframe(s, frame_invariant_create(b, s->stack));
}

void
state_pushloopframe(struct state *s, struct ast_block *b)
{
	state_pushframe(s, frame_loop_create(b, s->stack, state_copy(s)));
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

char *
state_rconst(struct state *s, char *key, bool persist)
{
	return rconst_declareorget(s->rconst, key, persist, s);
}

char *
state_rconstnokey(struct state *s, bool persist)
{
	return rconst_declarenokey(s->rconst, persist, s);
}

int
state_rconst_isanyint(struct state *s, char *rconst)
{
	return rconst_isanyint(s->rconst, rconst);
}

struct str_res *
state_getrconstwithvalue(struct state *s, int c)
{
	return rconst_getwithconstvalue(s->rconst, c);
}


struct error *
state_addconstraint(struct state *s, struct lsi_le *le)
{
	return rconst_addconstraint(s->rconst, le);
}

int
state_satisfies(struct state *s, struct lsi_le *le)
{
	return rconst_satisfies(s->rconst, le);
}

struct lsi_range *
state_range_eval(struct state *s, struct lsi_expr *e)
{
	return rconst_range_eval(s->rconst, e);
}

struct value *
state_popregister(struct state *state)
{
	/* 
	 * v_printf("reading from register: %s\n", state->reg ? value_str(state->reg) : "NULL");
	 */
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

void
state_break(struct state *s)
{
	assert(stack_inloop(s->stack));
	while (!stack_isloopbase(s->stack)) {
		state_popframe(s);
	}
	state_popframe(s);
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
	struct object_res *res = state_get(state, loc);
	assert(object_res_hasobject(res));
	object_assign(
		object_res_as_object(res),
		value_literal_create(dynamic_str(lit))
	);
	static_memory_stringpool(state->static_memory, lit, loc);
	return loc;
}

bool
state_loc_valid(struct state *state, struct location *loc)
{
	return location_tostatic(loc, state->static_memory)
		|| location_toheap(loc, state->heap)
		|| location_tostack(loc, state->stack)
		|| location_toclump(loc, state->clump);
}

bool
state_loc_onheap(struct state *state, struct location *loc)
{
	return location_toheap(loc, state->heap);
}

int
state_hasrconst(struct state *state, char *id)
{
	return rconst_hasvar(state->rconst, id);
}

struct value *
state_getrconst(struct state *state, char *id)
{
	assert(state_hasrconst(state, id));
	return value_rconst_create(ast_expr_identifier_create(dynamic_str(id)));
}

struct object_res *
state_get(struct state *state, struct location *loc)
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
		base_offset, state
	);
	char *structmember = offset_member(of);
	if (!structmember) {
		return obj_res;
	}

	struct ast_type *t = offset_membertype(of);
	struct object *root_obj = object_res_as_object(obj_res);
	struct object *member = object_getmember(
		root_obj, t, structmember, state
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

static struct lsi_varmap *
_stack_rconst_mapping(struct stack *stack, struct state *state);

static struct error *
_shapeverify_top(struct state *spec, struct state *impl);

struct error *
state_verify_callsetup(struct state *spec, struct state *impl)
{
	struct lsi_varmap *spec_lv = _stack_rconst_mapping(spec->stack, spec);
	struct lsi_varmap *impl_lv = _stack_rconst_mapping(impl->stack, impl);
	struct error *err = rconst_constraintverify(
		spec->rconst, impl->rconst, spec_lv, impl_lv
	);
	lsi_varmap_destroy(impl_lv);
	lsi_varmap_destroy(spec_lv);
	return err ? err : _shapeverify_top(spec, impl);
}

static struct lsi_varmap *
_var_rconst_mapping(struct state *, char *id);

static struct lsi_varmap *
_stack_rconst_mapping(struct stack *stack, struct state *state)
{
	int i;

	struct lsi_varmap *lv = lsi_varmap_create();

	struct map *m = stack_getlocalvars(stack);
	for (i = 0; i < m->n; i++)
		lsi_varmap_addrange(
			lv, _var_rconst_mapping(state, m->entry[i].key)
		);

	return lv;
}

static struct object *
location_mustgetobject(struct location *, struct state *);

static struct lsi_varmap *
_var_rconst_mapping(struct state *s, char *id)
{
	struct location *loc = loc_res_as_loc(state_getloc(s, id));
	struct ast_type *t = state_getvariabletype(s, id);
	if (ast_type_isarr(t)) {
		struct ast_type *ptr = ast_type_create_ptr(
			ast_type_arr_type(ast_type_copy(t))
		);
		struct lsi_varmap *lv = state_block_rconst_mapping(
			s, loc, t, id
		);
		ast_type_destroy(ptr);
		return lv;
	}
	struct object *obj = location_mustgetobject(loc, s);
	if (!object_isdef(obj)) {
		return lsi_varmap_create();
	}
	return value_rconst_mapping(
		object_as_defvalue(obj), t, s, id
	);
}

static struct object *
location_mustgetobject(struct location *loc, struct state *s)
{
	return object_res_as_object(state_get(s, loc));
}

static struct error *
_var_shapeverify(struct state *spec, struct state *impl, char *id);

static struct error *
_shapeverify_top(struct state *spec, struct state *impl)
{
	int i;

	struct map *varmap = stack_getlocalvars(spec->stack);
	for (i = 0; i < varmap->n; i++) {
		char *id = varmap->entry[i].key;
		struct error *err = _var_shapeverify(spec, impl, id);
		if (err) {
			return error_printf("invariant: %w", err);
		}
	}

	return NULL;
}

static struct error *
_var_shapeverify(struct state *spec, struct state *impl, char *id)
{
	struct constraint *c = constraint_create(
		dynamic_str(id),
		ast_type_copy(state_getvariabletype(spec, id)),
		spec, impl
	);
	struct error *err = constraint_shapeverify(
		c,
		location_mustgetobject(
			loc_res_as_loc(state_getloc(spec, id)), spec
		),
		location_mustgetobject(
			loc_res_as_loc(state_getloc(impl, id)), impl
		)
	);
	constraint_destroy(c);
	return err;
}

struct error *
state_shapeverify_structmember(struct state *spec, struct state *impl,
		struct value *spec_v, struct value *impl_v, char *member)
{
	struct object *spec_obj = value_struct_member(spec_v, member),
		      *impl_obj = value_struct_member(impl_v, member);
	assert(spec_obj && impl_obj);
	if (!spec_obj) {
		return NULL;
	}
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s.%s", "[PARENT]", member);
	struct constraint *c = constraint_create(
		strbuilder_build(b),
		ast_type_copy(value_struct_membertype(spec_v, member)),
		state_copy(spec),
		state_copy(impl)
	);
	struct error *err = constraint_shapeverify(c, spec_obj, impl_obj);
	constraint_destroy(c);
	return err;
}

struct lsi_varmap *
state_rconst_mapping_structmember(struct state *s, struct value *v, char *parent,
		char *member)
{
	struct object *obj = value_struct_member(v, member);
	if (!obj) {
		return lsi_varmap_create();
	}

	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s.%s", parent, member);
	char *id = strbuilder_build(b);

	struct lsi_varmap *lv = value_rconst_mapping(
		object_as_defvalue(obj),
		value_struct_membertype(v, member),
		s,
		id
	);

	free(id);

	return lv;
}

struct lsi_varmap *
state_block_rconst_mapping(struct state *s, struct location *loc,
		struct ast_type *t, char *referent)
{
	struct block *b = state_getblock(s, loc);
	assert(b);
	return block_rconst_mapping(b, t, s, referent);
}

static struct error *
_mutating_constraintverify_all(struct state *spec, struct state *impl);

struct error *
state_constraintverify_all(struct state *spec, struct state *impl)
{
	struct state *spec_copy = state_copy(spec),
		     *impl_copy = state_copy(impl);
	struct error *err = _mutating_constraintverify_all(spec_copy, impl_copy);
	state_destroy(impl_copy);
	state_destroy(spec_copy);
	return err;
}

static struct error *
_shapeverify_all(struct stack *spec_stack, struct state *spec,
		struct state *impl);

static struct error *
_mutating_constraintverify_all(struct state *spec, struct state *impl)
{
	struct lsi_varmap *spec_lv = _stack_rconst_mapping(spec->stack, spec);
	struct lsi_varmap *impl_lv = _stack_rconst_mapping(impl->stack, impl);
	struct error *err = rconst_constraintverify(
		spec->rconst, impl->rconst,
		spec_lv, impl_lv
	);
	lsi_varmap_destroy(impl_lv);
	lsi_varmap_destroy(spec_lv);
	if (err) {
		return err;
	}
	return _shapeverify_all(spec->stack, spec, impl);
}

static struct error *
_shapeverify_all(struct stack *spec_stack, struct state *spec,
		struct state *impl)
{
	struct error *err = _shapeverify_top(spec, impl);
	if (err) {
		return err;
	}
	if (stack_prev(spec_stack)) {
		return _shapeverify_all(stack_prev(spec_stack), spec, impl);
	}
	return NULL;
}

struct error *
state_verify_invariant(struct state *s)
{
	return stack_verify_invariant(s->stack, s);
}

int
state_isfeasible(struct state *s, struct lsi_le *le)
{
	return rconst_isfeasible(s->rconst, le);
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

struct ast_type *
state_getreturntype(struct state *s)
{
	return stack_returntype(s->stack);
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

	return state_get(state, variable_location(v));
}

struct object_res *
state_deref(struct state *state, struct value *ptr_val)
{
	if (value_isrconst(ptr_val)) {
		return object_res_error_create(error_state_deref_rconst());
	}
	struct location *deref = value_as_location(ptr_val);
	assert(deref);

	/* `*(ptr+offset)` */
	struct object_res *res = state_get(state, deref);
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
state_alloc(struct state *state, struct value *size)
{
	if (stack_insetup(state->stack)) {
		return heap_newcallerblock(state->heap, size);
	}
	return heap_newblock(state->heap, size);
}

struct value *
state_clump(struct state *state, struct value *size)
{
	/* XXX: should type be associated with blocks for type checking when we
	 * assign? */
	return value_ptr_create(
		location_create_dereferencable(
			clump_newblock(state->clump, size),
			offset_create(ast_expr_constant_create(0))
		)
	);
}



struct error *
state_dealloc(struct state *state, struct location *loc)
{
	return location_dealloc(loc, state->heap);
}

bool
state_addresses_deallocand(struct state *state, struct object *obj)
{
	return state_isdeallocand(
		state,
		value_as_location(object_as_defvalue(obj))
	);
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

static struct error *
_shape_and_constraint_verify(struct state *spec, struct state *impl);

struct error *
state_verify_endstate(struct state *impl, struct state *spec)
{
	struct state *impl_c = state_copy(impl),
		     *spec_c = state_copy(spec);
	if (spec->reg) {
		if (!impl->reg) {
			return error_printf("must have return value");
		}
		return _shape_and_constraint_verify(spec, impl);
	}
	assert(!impl->reg);
	state_destroy(spec_c);
	state_destroy(impl_c);
	return NULL;
}

static struct lsi_varmap *
_stack_and_return_mapping(struct state *, struct value *ret);

static struct error *
_shape_and_constraint_verify(struct state *spec, struct state *impl)
{
	struct state *spec_copy = state_copy(spec),
		     *impl_copy = state_copy(impl);
	struct constraint *c = constraint_create(
		dynamic_str("return"),
		ast_type_copy(stack_returntype(spec->stack)),
		spec_copy,
		impl_copy
	);
	struct error *err = constraint_shapeverify(
		c,
		object_create(
			value_int_create(0), spec->reg, value_int_create(1)
		),
		object_create(
			value_int_create(0), impl->reg, value_int_create(1)
		)
	);
	if (err) {
		return err;
	}
	struct lsi_varmap *spec_lv = _stack_and_return_mapping(
		spec_copy, spec->reg
	);
	struct lsi_varmap *impl_lv = _stack_and_return_mapping(
		impl_copy, impl->reg
	);
	err = rconst_constraintverify(
		spec_copy->rconst, impl_copy->rconst, spec_lv, impl_lv
	);
	lsi_varmap_destroy(impl_lv);
	lsi_varmap_destroy(spec_lv);
	constraint_destroy(c);
	return err;
}

static struct lsi_varmap *
_stack_and_return_mapping(struct state *s, struct value *ret)
{
	struct lsi_varmap *lv = _stack_rconst_mapping(s->stack, s);
	lsi_varmap_addrange(
		lv,
		value_rconst_mapping(
			ret,
			stack_returntype(s->stack),
			s,
			"return"
		)
	);
	return lv;
}


/* state_arr */

struct state_arr {
	int n;
	struct state **s;
};

struct state_arr *
state_arr_create(void)
{
	return calloc(1, sizeof(struct state_arr));
}

struct state_arr *
state_arr_copy(struct state_arr *old)
{
	int i;

	struct state_arr *new = state_arr_create();
	for (i = 0; i < old->n; i++) {
		state_arr_append(new, state_copy(old->s[i]));
	}
	return new;
}

void
state_arr_destroy(struct state_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		state_destroy(arr->s[i]);
	}
	free(arr);
}

void
state_arr_append(struct state_arr *arr, struct state *s)
{
	arr->s = realloc(arr->s, sizeof(struct state *) * ++arr->n);
	arr->s[arr->n-1] = s;
}

int
state_arr_len(struct state_arr *arr)
{
	return arr->n;
}

struct state **
state_arr_s(struct state_arr *arr)
{
	return arr->s;
}
