#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "util.h"
#include "state.h"

/* struct state */

struct heaploc;

struct heap;

struct heap *
heap_create();

void
heap_destroy(struct heap *h);

struct stack;

struct object;

bool
heap_referenced(struct heap *h, struct stack *stack);

struct heaploc *
heap_alloc(struct heap *h);

struct error *
heap_free(struct heap *h, struct heaploc *loc);


struct pointer;

struct pointer *
pointer_deref(struct pointer *ptr, int nderef);

struct heaploc *
pointer_heaploc_get(struct pointer *ptr, struct ast_expr *offset,
		struct state *state);

void
pointer_heaploc_assign(struct pointer *ptr, struct ast_expr *lower,
		struct ast_expr *upper, struct heaploc *loc);

bool
pointer_range_onheap(struct pointer *ptr, struct ast_expr *lower,
		struct ast_expr *upper);
struct error *
pointer_range_unalloc(struct pointer *ptr, struct ast_expr *lower,
		struct ast_expr *upper, struct state *state);

struct reference;

struct reference *
ref_create_result(int nderef, struct ast_expr *lower, struct ast_expr *upper);

bool
ref_isresult(struct reference *ref); 

struct ast_expr *
ref_lower(struct reference *ref);

struct ast_expr *
ref_upper(struct reference *ref);

char *
ref_id(struct reference *ref);

int
ref_nderef(struct reference *ref);

struct stack;

struct stack *
stack_create(struct stack *prev, struct ast_type *result_type);

void
stack_destroy(struct stack *stack);

bool
stack_references(struct stack *stack, int address);

struct stack *
stack_prev(struct stack *stack);

void
stack_declare(struct stack *stack, struct ast_variable *var, bool isparam);

struct ast_variable **
stack_getvariables(struct stack *stack);

int
stack_nvariables(struct stack *stack);

struct reference *
stack_result_create_ref(struct stack *stack, struct ast_expr *lower,
		struct ast_expr *upper);

struct object *
stack_getobject(struct stack *stack, Ref *ref);

struct object *
stack_result(struct stack *stack);

char *
stack_str(struct stack *stack);

struct pointer *
object_pointer(struct object *obj);

struct state {
	struct map *extfunc;
	struct heap *heap;
	struct stack *stack;
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

bool
stack_abstractly_equivalent(struct stack *s1, struct stack *s2);

bool
state_abstractly_equivalent(struct state *s1, struct state *s2)
{
	/* TODO: check that globals are equivalent */
	return stack_abstractly_equivalent(s1->stack, s2->stack);
}

bool
state_heap_referenced(struct state *state)
{
	/* checks: all allocated ranges on stack are parameters, globals, or
	 * result */
	return heap_referenced(state->heap, state->stack);
}

struct map *
state_extfunc(struct state *state)
{
	return state->extfunc;
}

void
state_pushframe(struct state *state, struct ast_type *type)
{
	state->stack = stack_create(state->stack, type);
}

void
state_popframe(struct state *state)
{
	struct stack *old = state->stack;
	state->stack = stack_prev(old); /* pop */
	/* destroy frame */
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

struct ast_function *
state_getfunc(struct state *state, char *f)
{
	struct ast_function *sym = map_get(state->extfunc, f);
	assert(sym);
	return sym;
}

bool
state_ref_onheap(struct state *state, struct reference *ref)
{
	struct object *obj = stack_getobject(state->stack, ref);
	assert(obj);
	struct pointer *ptr = pointer_deref(
		object_pointer(obj), ref_nderef(ref)
	);
	assert(ptr);
	return pointer_range_onheap(ptr, ref_lower(ref), ref_upper(ref));
}

struct heaploc *
state_ref_get_heaploc(struct state *state, Ref *ref)
{
	struct object *obj = stack_getobject(state->stack, ref);
	assert(obj);
	struct pointer *ptr = pointer_deref(
		object_pointer(obj), ref_nderef(ref)
	);
	assert(ptr);
	return pointer_heaploc_get(ptr, ref_lower(ref), state);
}

char *
heaploc_str(struct heaploc *loc);

void
state_ref_assign_heaploc(struct state *state, struct reference *ref,
		struct heaploc *loc)
{
	struct object *obj = stack_getobject(state->stack, ref);
	assert(obj);
	struct pointer *ptr = pointer_deref(
		object_pointer(obj), ref_nderef(ref)
	);
	assert(ptr);

	/* TODO: check ref is single index with arithmetic engine */
	pointer_heaploc_assign(ptr, ref_lower(ref), ref_upper(ref), loc);
}

struct error *
state_ref_unalloc(struct state *state, struct reference *ref)
{
	struct object *obj = stack_getobject(state->stack, ref);
	assert(obj);
	struct pointer *ptr = pointer_deref(
		object_pointer(obj), ref_nderef(ref)
	);
	assert(ptr);
	return pointer_range_unalloc(ptr, ref_lower(ref), ref_upper(ref), state);
}

void
state_result_assign(struct state *state, struct heaploc *loc)
{
	Ref *result = state_getresult(state);

	state_ref_assign_heaploc(state, result, loc);

	ref_destroy(result);
}

struct reference *
state_getresult(struct state *state)
{
	/* XXX: allow assigning to ranges/offsets of result */
	struct ast_expr *lower = ast_expr_create_constant(0),
			*upper = ast_expr_create_constant(1);
	struct reference *result = stack_result_create_ref(
		state->stack, lower, upper
	);
	ast_expr_destroy(upper);
	ast_expr_destroy(lower);

	return result;
}

struct heaploc *
state_alloc(struct state *state)
{
	return heap_alloc(state->heap);
}

struct error *
state_heaploc_free(struct state *state, struct heaploc *loc)
{
	return heap_free(state->heap, loc);
}

char *
heap_str(struct heap *);

char *
state_str(struct state *state)
{
	struct strbuilder *b = strbuilder_create();
	char *stack = stack_str(state->stack);
	char *heap = heap_str(state->heap);
	strbuilder_printf(b, "[[\n%s\n\t[%s]\n]]\n", stack, heap);
	free(heap);
	free(stack);
	return strbuilder_build(b);
}


/* struct heap */

struct heaploc;

struct heaploc *
heaploc_create(int address);

void
heaploc_destroy(struct heaploc *loc);

struct error *
heaploc_free(struct heaploc *loc);

int
heaploc_address(struct heaploc *loc);

bool
heaploc_isfreed(struct heaploc *loc);

/* XXX: should this be a typedef of a heaploc_arr? */
struct heap {
	int n;
	struct heaploc **loc;
};

struct heap *
heap_create()
{
	struct heap *h = calloc(1, sizeof(struct heap));
	assert(h);
	return h;
}

void
heap_destroy(struct heap *h)
{
	for (int i = 0; i < h->n; i++) {
		heaploc_destroy(h->loc[i]);
	}
	free(h->loc);
	free(h);
}

char *
heap_str(struct heap *h)
{
	struct strbuilder *b = strbuilder_create();
	for (int i = 0; i < h->n; i++) {
		strbuilder_printf(b, "%s%s", heaploc_str(h->loc[i]),
			i+1 < h->n ? ", " : "");
	}
	return strbuilder_build(b);
}

bool
heap_referenced(struct heap *h, struct stack *stack)
{
	for (int i = 0; i < h->n; i++) {
		struct heaploc *loc = h->loc[i];
		if (heaploc_isfreed(loc)) {
			continue;
		}
		if (!stack_references(stack, heaploc_address(loc))) {
			return false;
		}
	}
	return true;
}

struct heaploc *
heap_alloc(struct heap *h)
{
	h->loc = realloc(h->loc, sizeof(struct heaploc *) * ++h->n);
	assert(h->loc);
	int addr = h->n-1;
	h->loc[addr] = heaploc_create(addr);
	return h->loc[addr];
}

struct error *
heap_free(struct heap *h, struct heaploc *loc)
{
	return heaploc_free(loc);
}


struct heaploc {
	int address;
	bool freed;
};

struct heaploc *
heaploc_create(int address)
{
	struct heaploc *loc = malloc(sizeof(struct heaploc));
	assert(loc);
	loc->address = address;
	loc->freed = false;
	return loc;
}

void
heaploc_destroy(struct heaploc *loc)
{
	free(loc);
}

int
heaploc_address(struct heaploc *loc)
{
	return loc->address;
}

struct error *
heaploc_free(struct heaploc *loc)
{
	if (loc->freed) {
		return error_create("double free");
	}
	loc->freed = true;
	return NULL;
}

bool
heaploc_isfreed(struct heaploc *loc)
{
	return loc->freed;
}

char *
heaploc_str(struct heaploc *loc)
{
	struct strbuilder *b = strbuilder_create();
	if (loc) {
		strbuilder_printf(b, "#%d:%d", loc->address, loc->freed);
	} else {
		strbuilder_printf(b, "nil");
	}
	return strbuilder_build(b);
}


/* struct stack */

char *
pointer_str(struct pointer *ptr);

struct object;

struct object *
object_create(struct ast_type *type, bool isparam);

void
object_destroy(struct object *obj);

struct ast_type *
object_type(struct object *obj);

struct pointer *
object_pointer(struct object *obj);

bool
object_heap_equivalent(struct object *obj1, struct object *obj2);

bool
object_references(struct object *obj, int address);

bool
object_isparam(struct object *obj);

char *
object_str(struct object *);

int
nderef_fromtype(struct ast_type *t);

struct stack {
	struct map *objmap;

	struct object *result;

	struct stack *prev; /* link register */
};

struct stack *
stack_create(struct stack *prev, struct ast_type *result_type)
{
	struct stack *stack = calloc(1, sizeof(struct stack));
	assert(stack);
	stack->objmap = map_create();
	stack->prev = prev;
	stack->result = object_create(result_type, false);
	return stack;
}

void
stack_destroy(struct stack *stack)
{
	struct map *m = stack->objmap;
	for (int i = 0; i < m->n; i++) {
		object_destroy((struct object *) m->entry[i].value);
	}
	map_destroy(m);
	object_destroy(stack->result);
	free(stack);
}

bool
stack_abstractly_equivalent(struct stack *s1, struct stack *s2)
{
	struct object *r1 = stack_result(s1),
		      *r2 = stack_result(s2);
	if (!object_heap_equivalent(r1, r2)) {
		return false;
	}
	struct map *m1 = s1->objmap,
		   *m2 = s2->objmap;
	assert(m1->n == m2->n);
	for (int i = 0; i < m1->n; i++) {
		struct entry e1 = m1->entry[i],
			     e2 = m2->entry[i];
		assert(strcmp(e1.key, e2.key) == 0);
		struct object *obj1 = (struct object *) e1.value,
			      *obj2 = (struct object *) e2.value;
		assert(object_isparam(obj1) == object_isparam(obj2));
		if (object_isparam(obj1) && !object_heap_equivalent(obj1, obj2)) {
			return false;
		}
	}
	return true;
}

struct reference *
stack_result_create_ref(struct stack *stack, struct ast_expr *lower,
		struct ast_expr *upper)
{
	return ref_create_result(
		nderef_fromtype(object_type(stack->result)), lower, upper
	);
}

struct object *
stack_result(struct stack *stack)
{
	return stack->result;
}

struct stack *
stack_prev(struct stack *stack)
{
	return stack->prev;
}

bool
stack_references(struct stack *stack, int address)
{
	if (object_references(stack_result(stack), address)) {
		return true;
	}

	struct map *m = stack->objmap;
	for (int i = 0; i < m->n; i++) {
		struct object *obj = (struct object *) m->entry[i].value;
		if (object_isparam(obj) && object_references(obj, address)) {
			return true;
		}
	}

	/* TODO: stack->prev for globals? */

	return false;
}

void
stack_declare(struct stack *stack, struct ast_variable *var, bool isparam)
{
	struct ast_type *t = ast_variable_type(var);
	char *id = ast_variable_name(var);
	assert(!map_get(stack->objmap, id));
	map_set(
		stack->objmap,
		dynamic_str(id),
		object_create(t, isparam)
	);
}

struct ast_variable **
stack_getvariables(struct stack *stack)
{
	struct map *m = stack->objmap;

	struct ast_variable **var = malloc(sizeof(struct ast_variable *) * m->n);
	assert(var);
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];

		struct object *obj = (struct object *) e.value;
		assert(obj);

		struct ast_type *t = object_type(obj); 
		var[i] = ast_variable_create(
			dynamic_str(e.key),
			ast_type_copy(t)
		);
	}
	return var;
}

int
stack_nvariables(struct stack *stack)
{
	return stack->objmap->n;
}

struct object *
stack_getobject(struct stack *stack, Ref *ref)
{
	if (ref_isresult(ref)) {
		return stack->result;
	}

	char *key = ref_id(ref);
	struct object *obj = map_get(stack->objmap, key);
	if (obj) {
		return obj;
	}
	if (stack->prev) {
		return stack_getobject(stack->prev, ref);
	}
	return NULL;
}

void
stack_assign(struct stack *stack, char *id, struct heaploc *loc)
{
	map_set(stack->objmap, dynamic_str(id), loc);
}

char *
stack_str(struct stack *stack)
{
	struct strbuilder *b = strbuilder_create();
	struct map *m = stack->objmap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		char *obj = object_str((struct object *) e.value);
		strbuilder_printf(b, "\t%s: %s", e.key, obj);
		free(obj);
		strbuilder_putc(b, '\n');
	}
	char *result = object_str(stack->result);
	strbuilder_printf(b, "\tresult: %s\n", result);
	free(result);
	if (stack->prev) {
		strbuilder_printf(b, "\t");
		char *prev = stack_str(stack->prev);
		/* TODO: fix length of line */
		for (int i = 0, len = strlen(prev); i < len-2; i++ ) {
			strbuilder_putc(b, '-');
		}
		strbuilder_printf(b, "\n");
		strbuilder_printf(b, prev);
		free(prev);
	}
	return strbuilder_build(b);
}

/* struct object */

struct pointer *
pointer_create(int nderef);

void
pointer_destroy(struct pointer *ptr);

bool
pointer_heap_equivalent(struct pointer *, struct pointer *);

bool
pointer_references(struct pointer *ptr, int address);

struct object {
	bool isparam;
	struct ast_type *type;
	struct pointer *ptr; /* reference to declared pointer */
};

struct object *
object_create(struct ast_type *type, bool isparam)
{
	struct object *obj = calloc(1, sizeof(struct object));
	assert(obj);
	obj->isparam = isparam;
	obj->type = ast_type_copy(type);
	/* obj->ptr defaults to NULL because of calloc */
	if (ast_type_base(type) == TYPE_POINTER) {
		obj->ptr = pointer_create(nderef_fromtype(type));
	}
	return obj;
}

void
object_destroy(struct object *obj)
{
	if (obj->ptr) {
		assert(ast_type_base(obj->type) == TYPE_POINTER);
		pointer_destroy(obj->ptr);
	}
	ast_type_destroy(obj->type);
	free(obj);
}

bool
object_isparam(struct object *obj)
{
	return obj->isparam;
}

struct ast_type *
object_type(struct object *obj)
{
	return obj->type;
}

struct pointer *
object_pointer(struct object *obj)
{
	assert(obj->ptr);
	return obj->ptr;
}

bool
object_ispointer(struct object *obj)
{
	return obj->ptr; /* cast */
}

bool
object_heap_equivalent(struct object *obj1, struct object *obj2)
{
	assert(obj1 && obj2);
	/* TODO: assert(obj1->type == obj2->type) */
	return pointer_heap_equivalent(obj1->ptr, obj2->ptr);
}

bool
object_references(struct object *obj, int address)
{
	return object_ispointer(obj) && pointer_references(object_pointer(obj), address);
}


char *
object_str(struct object *obj)
{
	struct strbuilder *b = strbuilder_create();
	if (object_ispointer(obj)) {
		char *s = pointer_str(object_pointer(obj));
		strbuilder_printf(b, "%s", s);
		free(s);
	} else {
		strbuilder_printf(b, "{}");
	}
	return strbuilder_build(b);
}

/* struct pointer */

typedef struct alloc_arr predicate;

predicate *
predicate_create();

void
predicate_destroy(predicate *);

int
predicate_memory_len(predicate *);

struct alloc **
predicate_alloc(predicate *);

struct heaploc *
predicate_heaploc_get(predicate *pred, struct ast_expr *offset,
		struct state *state);

void
predicate_heaploc_assign(predicate *pred, struct ast_expr *lower,
		struct ast_expr *upper, struct heaploc *loc);

bool
predicate_range_onheap(predicate *, struct ast_expr *lower,
		struct ast_expr *upper);

struct error *
predicate_unalloc(predicate *pred, struct ast_expr *lower,
		struct ast_expr *upper, struct state *state);

bool
predicate_references(predicate *pred, int address);

bool
predicate_heap_equivalent(predicate *pred1, predicate *pred2);

char *
predicate_str(predicate *pred);

struct pointer {
	predicate *pred;
	struct pointer *deref;
};

int
nderef_fromtype(struct ast_type *t);

struct pointer *
pointer_create(int nderef)
{
	struct pointer *ptr = calloc(1, sizeof(struct pointer));
	assert(ptr);
	if (nderef > 0) {
		ptr->deref = pointer_create(nderef - 1);
	}
	ptr->pred = predicate_create();
	return ptr;
}

int
nderef_fromtype(struct ast_type *t)
{
	if (ast_type_base(t) == TYPE_POINTER) {
		return 1 + nderef_fromtype(ast_type_ptr_type(t));
	}
	return 0;
}

void
pointer_destroy(struct pointer *ptr)
{
	predicate_destroy(ptr->pred);
	if (ptr->deref) {
		pointer_destroy(ptr->deref);
	}
	free(ptr);
}

bool
pointer_heap_equivalent(struct pointer *ptr1, struct pointer *ptr2)
{
	/* order ptr1, ptr2 so that ptr1 is not NULL, or return true if both are
	 * NULL */
	if (!ptr1) {
		if (!ptr2) {
			return true;
		}
		return pointer_heap_equivalent(ptr2, ptr1);
	}
	assert(ptr1);

	if (!ptr2 || !predicate_heap_equivalent(ptr1->pred, ptr2->pred)) {
		return false;
	}

	return pointer_heap_equivalent(ptr1->deref, ptr2->deref);
}

struct pointer *
pointer_deref(struct pointer *ptr, int nderef)
{
	if (nderef > 0) {
		assert(ptr);
		return pointer_deref(ptr->deref, nderef - 1);
	}
	return ptr;
}

bool
pointer_references(struct pointer *ptr, int address)
{
	if (predicate_references(ptr->pred, address)) {
		return true;
	}
	if (ptr->deref) {
		return pointer_references(ptr->deref, address);
	}
	return false;
}

struct heaploc *
pointer_heaploc_get(struct pointer *ptr, struct ast_expr *offset,
		struct state *state)
{
	return predicate_heaploc_get(ptr->pred, offset, state);
}

void
pointer_heaploc_assign(struct pointer *ptr, struct ast_expr *lower,
		struct ast_expr *upper, struct heaploc *loc)
{
	return predicate_heaploc_assign(ptr->pred, lower, upper, loc);
}

bool
pointer_range_onheap(struct pointer *ptr, struct ast_expr *lower,
		struct ast_expr *upper)
{
	return predicate_range_onheap(ptr->pred, lower, upper);
}

struct error *
pointer_range_unalloc(struct pointer *ptr, struct ast_expr *lower,
		struct ast_expr *upper, struct state *state)
{
	return predicate_unalloc(ptr->pred, lower, upper, state);
}

char *
pointer_str(struct pointer *ptr)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "{ ");
	char *pred = predicate_str(ptr->pred);
	strbuilder_printf(b, "%s", pred);
	free(pred);
	if (ptr->deref) {
		char *deref = pointer_str(ptr->deref);
		strbuilder_printf(b, "; *%s", deref);
		free(deref);
	}
	strbuilder_printf(b, " }");
	return strbuilder_build(b);
}

/* predicate */

struct alloc;

struct alloc *
alloc_create(struct ast_expr *lower, struct ast_expr *upper,
		struct heaploc *loc);

void
alloc_destroy(struct alloc *b);

struct heaploc *
alloc_heaploc(struct alloc *b);

struct ast_expr *
alloc_lower(struct alloc *);

struct ast_expr *
alloc_upper(struct alloc *);

bool
alloc_empty(struct alloc *b);

bool
alloc_singular(struct alloc *b);

bool
alloc_contains(struct alloc *b, struct ast_expr *offset);

bool
alloc_contains_upperincl(struct alloc *b, struct ast_expr *offset);

bool
alloc_contig_precedes(struct alloc *b, struct alloc *other);

struct alloc *
alloc_upto(struct alloc *b, struct ast_expr *excl_upper, struct state *state);

struct alloc *
alloc_from(struct alloc *b, struct ast_expr *incl_lower, struct state *state);

char *
alloc_str(struct alloc *b);

struct alloc_arr;

struct alloc_arr *
alloc_arr_create();

void
alloc_arr_destroy(struct alloc_arr *arr);

struct alloc **
alloc_arr_allocs(struct alloc_arr *arr);

int
alloc_arr_nallocs(struct alloc_arr *arr);

int
alloc_arr_index(struct alloc_arr *arr, struct ast_expr *offset);

int
alloc_arr_index_upperincl(struct alloc_arr *arr, struct ast_expr *offset);

void
alloc_arr_insert(struct alloc_arr *arr, int index, struct alloc *b);

void
alloc_arr_append(struct alloc_arr *arr, struct alloc *b);

void
alloc_arr_delete(struct alloc_arr *arr, int index);

predicate *
predicate_create()
{
	return alloc_arr_create();
}

void
predicate_destroy(predicate *pred)
{
	alloc_arr_destroy(pred);
}

struct heaploc *
predicate_observe(predicate *pred, struct ast_expr *offset, struct state *state);

struct heaploc *
predicate_heaploc_get(predicate *pred, struct ast_expr *offset, struct state *state)
{
	int index = alloc_arr_index(pred, offset);
	if (index == -1) {
		return NULL;
	}

	struct alloc *b = alloc_arr_allocs(pred)[index];
	if (alloc_singular(b)) { /* already observed */
		return alloc_heaploc(b);
	}

	return predicate_observe(pred, offset, state);
}

struct heaploc *
predicate_observe(predicate *pred, struct ast_expr *observand, struct state *state)
{
	int index = alloc_arr_index(pred, observand);
	assert(index != -1);

	/* range around observand */
	struct ast_expr *lower = ast_expr_copy(observand);
	struct ast_expr *upper = ast_expr_create_binary(
		ast_expr_copy(observand),
		BINARY_OP_ADDITION,
		ast_expr_create_constant(1)
	);

	struct heaploc *loc = state_alloc(state);

	/* obtain from, observed, upto */
	struct alloc *b = alloc_arr_allocs(pred)[index];
	struct alloc *upto = alloc_upto(b, lower, state),
		     *from = alloc_from(b, upper, state),
		     *observed = alloc_create(lower, upper, loc);

	struct error *err = heaploc_free(alloc_heaploc(b));
	assert(!err);
	alloc_arr_delete(pred, index);

	/* destroy range around observand */
	ast_expr_destroy(upper);
	ast_expr_destroy(lower);

	if (upto) {
		alloc_arr_insert(pred, index++, upto);
	} 
	alloc_arr_insert(pred, index++, observed);
	if (from) {
		alloc_arr_insert(pred, index, from);
	}

	return loc;
}

void
predicate_heaploc_assign(predicate *pred, struct ast_expr *lower,
		struct ast_expr *upper, struct heaploc *loc)
{
	assert(alloc_arr_nallocs(pred) == 0); /* XXX */

	alloc_arr_append(pred, alloc_create(lower, upper, loc));
}

bool
predicate_range_onheap(predicate *pred, struct ast_expr *lower, struct ast_expr *upper)
{
	assert(!ast_expr_equal(lower, upper));

	int lw_index = alloc_arr_index(pred, lower);
	if (lw_index == -1) {
		return false;
	}

	int up_index = alloc_arr_index_upperincl(pred, upper);
	if (up_index == -1) {
		return false;
	}
	
	struct alloc **alloc = alloc_arr_allocs(pred);
	for (int i = lw_index; i < up_index; i++) {
		if (!alloc_contig_precedes(alloc[i], alloc[i+1])) {
			return false;
		}
	}
	return true;
}

struct error *
predicate_unalloc(predicate *pred, struct ast_expr *lower,
		struct ast_expr *upper, struct state *state)
{
	/* checking that lower bound alloced */
	int lw_index = alloc_arr_index(pred, lower);
	if (lw_index == -1) {
		return error_create("lower bound not allocated");
	}

	int up_index = alloc_arr_index_upperincl(pred, upper);
	if (up_index == -1) {
		return error_create("upper bound not allocated");
	}

	struct alloc **alloc = alloc_arr_allocs(pred);

	struct alloc *upto = alloc_upto(alloc[lw_index], lower, state),
		     *from = alloc_from(alloc[up_index], upper, state);

	for (int i = lw_index; i <= up_index; i++) {
		struct error *err = heaploc_free(alloc_heaploc(alloc[i]));
		if (err) {
			return err;
		}
		alloc_arr_delete(pred, i);
	}

	/* XXX */
	if (upto) {
		alloc_arr_insert(pred, lw_index++, upto);
	}
	if (from) {
		alloc_arr_insert(pred, lw_index, from);
	}

	return NULL;
}

bool
predicate_references(predicate *pred, int address)
{
	struct alloc **alloc = alloc_arr_allocs(pred);
	int nallocs = alloc_arr_nallocs(pred);
	for (int i = 0; i < nallocs; i++) {
		struct heaploc *loc = alloc_heaploc(alloc[i]);
		if (loc && heaploc_address(loc) == address) {
			return true;
		}
	}
	return false;
}

bool
predicate_heap_subequivalent(predicate *pred, predicate *sub);

bool
predicate_heap_equivalent(predicate *pred1, predicate *pred2)
{
	return predicate_heap_subequivalent(pred1, pred2)
		&& predicate_heap_subequivalent(pred2, pred1);
}

bool
predicate_heap_subequivalent(predicate *pred, predicate *sub)
{
	struct alloc **alloc = alloc_arr_allocs(sub);
	int nallocs = alloc_arr_nallocs(sub);
	for (int i = 0; i < nallocs; i++) {
		struct alloc *b = alloc[i];
		if (!predicate_range_onheap(pred, alloc_lower(b), alloc_upper(b))) {
			return heaploc_isfreed(alloc_heaploc(b));
		}
	}
	return true;
}


int
count_unfreed(predicate *pred);

char *
predicate_str(predicate *pred)
{
	struct strbuilder *sb = strbuilder_create();
	int printed = 0,
	    unfreed = count_unfreed(pred);
	struct alloc **alloc = alloc_arr_allocs(pred);
	int nallocs = alloc_arr_nallocs(pred);
	for (int i = 0; i < nallocs; i++) {
		struct alloc *b = alloc[i];
		struct heaploc *loc = alloc_heaploc(b);
		if (loc && heaploc_isfreed(loc)) {
			continue;
		}
		printed++;
		char *block = alloc_str(b);
		strbuilder_printf(sb,
			"%s%s",
			block,
			(printed < unfreed) ? ", " : ""
		);
		free(block);
	}
	return strbuilder_build(sb);
}

int
count_unfreed(predicate *pred)
{
	struct alloc **alloc = alloc_arr_allocs(pred);
	int nallocs = alloc_arr_nallocs(pred);
	int num = nallocs;
	for (int i = 0; i < nallocs; i++) {
		struct heaploc *loc = alloc_heaploc(alloc[i]);
		if (loc && heaploc_isfreed(loc)) {
			num--;
		}
	}
	return num;
}


struct alloc_arr {
	int n;
	struct alloc **alloc;
};

struct alloc_arr *
alloc_arr_create()
{
	struct alloc_arr *arr = calloc(1, sizeof(struct alloc_arr));
	assert(arr);
	return arr;
}

void
alloc_arr_destroy(struct alloc_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		alloc_destroy(arr->alloc[i]);
	}
	free(arr->alloc);
	free(arr);
}

struct alloc **
alloc_arr_allocs(struct alloc_arr *arr)
{
	return arr->alloc;
}

int
alloc_arr_nallocs(struct alloc_arr *arr)
{
	return arr->n;
}

int
alloc_arr_index(struct alloc_arr *arr, struct ast_expr *offset)
{
	for (int i = 0; i < arr->n; i++) {
		if (alloc_contains(arr->alloc[i], offset)) {
			return i;
		}
	}
	return -1;
}

int
alloc_arr_index_upperincl(struct alloc_arr *arr, struct ast_expr *offset)
{
	for (int i = 0; i < arr->n; i++) {
		if (alloc_contains_upperincl(arr->alloc[i], offset)) {
			return i;
		}
	}
	return -1;
}

void
alloc_arr_insert(struct alloc_arr *arr, int index, struct alloc *b)
{
	assert(!alloc_empty(b));

	arr->alloc = realloc(arr->alloc, sizeof(struct alloc *) * ++arr->n);
	assert(arr->alloc);
	for (int i = arr->n-1; i > index; i--) {
		arr->alloc[i] = arr->alloc[i-1];
	}
	arr->alloc[index] = b;
}

void
alloc_arr_append(struct alloc_arr *arr, struct alloc *b)
{
	return alloc_arr_insert(arr, arr->n, b);
}

void
alloc_arr_delete(struct alloc_arr *arr, int index)
{
	alloc_destroy(arr->alloc[index]);
	for (int i = index; i < arr->n-1; i++) {
		arr->alloc[i] = arr->alloc[i+1];
	}
	arr->alloc = realloc(arr->alloc, sizeof(struct alloc *) * --arr->n);
	assert(arr->alloc || !arr->n);
}


struct alloc {
	struct ast_expr *lower, *upper;
	struct heaploc *loc; /* only valid if upper - lower == 1 */
};

struct alloc *
alloc_create(struct ast_expr *lower, struct ast_expr *upper,
		struct heaploc *loc)
{
	struct alloc *b = malloc(sizeof(struct alloc));
	assert(b);
	b->lower = ast_expr_copy(lower);
	b->upper = ast_expr_copy(upper);
	assert(loc);
	b->loc = loc;
	return b;
}

void
alloc_destroy(struct alloc *b)
{
	/* b->loc is handled by state_destroy */
	ast_expr_destroy(b->lower);
	ast_expr_destroy(b->upper);
	free(b);
}

struct ast_expr *
calc_simplify(struct ast_expr *expr);

bool
alloc_empty(struct alloc *b)
{
	struct ast_expr *lw = calc_simplify(b->lower),
			*up = calc_simplify(b->upper);
	bool empty = ast_expr_equal(lw, up);
	ast_expr_destroy(lw);
	ast_expr_destroy(up);
	return empty;
}

bool
hack_is_i_to_i_plus_one(struct alloc *b);

bool
alloc_singular(struct alloc *b)
{
	if (hack_is_i_to_i_plus_one(b)) {
		return true;
	}
	struct ast_expr *simp_lw = calc_simplify(b->lower),
			*simp_up = calc_simplify(b->upper);
	int lw = ast_expr_as_constant(simp_lw),
	    up = ast_expr_as_constant(simp_up);
	bool singular = up-lw == 1;
	ast_expr_destroy(simp_lw);
	ast_expr_destroy(simp_up);
	return singular;
}

bool
hack_is_i_to_i_plus_one(struct alloc *b)
{
	if (ast_expr_kind(b->lower) != EXPR_IDENTIFIER) {
		return false;
	}
	if (strcmp(ast_expr_as_identifier(b->lower), "i") != 0) {
		return false;
	}
	struct ast_expr *up = b->upper;
	if (ast_expr_kind(up) != EXPR_BINARY) {
		return false;
	}
	struct ast_expr *e1 = ast_expr_binary_e1(up),
			*e2 = ast_expr_binary_e2(up);
	if (ast_expr_kind(e2) != EXPR_CONSTANT) {
		return false;
	}
	return ast_expr_equal(b->lower, e1)
		&& ast_expr_binary_op(up) == BINARY_OP_ADDITION
		&& ast_expr_as_constant(e2) == 1;
}

bool
hack_is_i_in_i_to_i_plus_one(struct alloc *b, struct ast_expr *offset);

bool
hack_is_i_plus_one_in_i_to_i_plus_one(struct alloc *b, struct ast_expr *offset);

struct alloc *
alloc_upto(struct alloc *b, struct ast_expr *excl_upper, struct state *state)
{
	if (hack_is_i_in_i_to_i_plus_one(b, excl_upper)) {
		return NULL;
	}

	struct ast_expr *simp_lw = calc_simplify(b->lower),
			*simp_up = calc_simplify(b->upper),
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

	if (up == excl_up) {
		return alloc_create(b->lower, excl_upper, alloc_heaploc(b));
	}
	return alloc_create(b->lower, excl_upper, state_alloc(state));
}

struct alloc *
alloc_from(struct alloc *b, struct ast_expr *incl_lower, struct state *state)
{
	if (hack_is_i_plus_one_in_i_to_i_plus_one(b, incl_lower)) {
		return NULL;
	}

	struct ast_expr *simp_lw = calc_simplify(b->lower),
			*simp_up = calc_simplify(b->upper),
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

	if (lw == incl_lw) {
		return alloc_create(incl_lower, b->upper, alloc_heaploc(b));
	}
	return alloc_create(incl_lower, b->upper, state_alloc(state));
}

struct ast_expr *
alloc_lower(struct alloc *b)
{
	return b->lower;
}

struct ast_expr *
alloc_upper(struct alloc *b)
{
	return b->upper;
}

struct heaploc *
alloc_heaploc(struct alloc *b)
{
	assert(b->loc);
	return b->loc;
}

bool
alloc_contains(struct alloc *b, struct ast_expr *offset)
{
	struct heaploc *loc = alloc_heaploc(b);
	if (loc && heaploc_isfreed(loc)) {
		return false;
	}

	if (hack_is_i_in_i_to_i_plus_one(b, offset)) {
		return true;
	}

	struct ast_expr *simp_lw = calc_simplify(b->lower),
			*simp_up = calc_simplify(b->upper),
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
hack_is_i_in_i_to_i_plus_one(struct alloc *b, struct ast_expr *offset)
{
	if (ast_expr_kind(offset) != EXPR_IDENTIFIER) {
		return false;
	}
	if (strcmp(ast_expr_as_identifier(offset), "i") != 0) {
		return false;
	}
	if (!ast_expr_equal(b->lower, offset)) {
		return false;
	}
	struct ast_expr *up = b->upper;
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

bool
hack_is_i_plus_one_in_i_to_i_plus_one(struct alloc *b, struct ast_expr *offset)
{
	/* lower is `i' */
	struct ast_expr *lw = b->lower;
	if (ast_expr_kind(lw) != EXPR_IDENTIFIER) {
		return false;
	}
	if (strcmp(ast_expr_as_identifier(lw), "i") != 0) {
		return false;
	}

	/* offset, upper are both `i+1' */
	if (!ast_expr_equal(offset, b->upper)) {
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
alloc_contains_upperincl(struct alloc *b, struct ast_expr *offset)
{
	struct heaploc *loc = alloc_heaploc(b);
	if (loc && heaploc_isfreed(loc)) {
		return false;
	}

	if (hack_is_i_plus_one_in_i_to_i_plus_one(b, offset)) {
		return true;
	}

	struct ast_expr *simp_lw = calc_simplify(b->lower),
			*simp_up = calc_simplify(b->upper),
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
alloc_contig_precedes(struct alloc *before, struct alloc *after)
{
	if (ast_expr_equal(before->upper, after->lower)) {
		return true;
	}
	return false;
}

char *
alloc_str(struct alloc *b)
{
	struct strbuilder *sb = strbuilder_create();
	strbuilder_printf(sb, "[");
	char *lw = ast_expr_str(b->lower),
	     *up = ast_expr_str(b->upper);
	strbuilder_printf(sb, "%s:%s", lw, up);
	free(up);
	free(lw);
	strbuilder_printf(sb, "]");
	char *loc = heaploc_str(b->loc);
	strbuilder_printf(sb, " @ %s", loc);
	free(loc);
	return strbuilder_build(sb);
}


struct reference {
	bool isresult;
	char *id;
	int nderef;
	struct ast_expr *lower, *upper;
};

struct reference *
ref_create_result(int nderef, struct ast_expr *lower, struct ast_expr *upper)
{
	struct reference *ref = malloc(sizeof(struct reference));
	assert(ref);
	ref->isresult = true;
	ref->nderef = nderef;
	ref->lower = ast_expr_copy(lower);
	ref->upper = ast_expr_copy(upper);
	return ref;
}

struct reference *
ref_create_range(char *id, int nderef, struct ast_expr *lower,
		struct ast_expr *upper)
{
	struct reference *ref = malloc(sizeof(struct reference));
	assert(ref);
	ref->isresult = false;
	ref->id = dynamic_str(id);
	ref->nderef = nderef;
	ref->lower = ast_expr_copy(lower);
	ref->upper = ast_expr_copy(upper);
	return ref;
}

struct reference *
ref_create_offset(char *id, int nderef, struct ast_expr *offset)
{
	struct ast_expr *upper = ast_expr_create_binary(
		ast_expr_copy(offset),
		BINARY_OP_ADDITION,
		ast_expr_create_constant(1)
	);
	struct reference *ref = ref_create_range(id, nderef, offset, upper);
	ast_expr_destroy(upper);
	return ref;
}

void
ref_destroy(struct reference *ref)
{
	if (!ref->isresult) {
		free(ref->id);
	}
	ast_expr_destroy(ref->lower);
	ast_expr_destroy(ref->upper);
	free(ref);
}

bool
ref_isresult(struct reference *ref)
{
	return ref->isresult;
}

char *
ref_id(struct reference *ref)
{
	assert(!ref->isresult);
	return ref->id;
}

int
ref_nderef(struct reference *ref)
{
	return ref->nderef;
}

struct ast_expr *
ref_lower(struct reference *ref)
{
	return ref->lower;
}

struct ast_expr *
ref_upper(struct reference *ref)
{
	return ref->upper;
}

char *
ref_str(Ref *ref)
{
	struct strbuilder *b = strbuilder_create();
	for (int i = 0; i < ref->nderef; i++) {
		strbuilder_putc(b, '*');
	}
	if (ref->isresult) {
		strbuilder_printf(b, "%s", KEYWORD_RESULT);
	} else {
		strbuilder_printf(b, "(%s)", ref->id);
	}
	char *lw = ast_expr_str(ref->lower),
	     *up = ast_expr_str(ref->upper);
	strbuilder_printf(b, "[%s:%s]", lw, up);
	free(up);
	free(lw);
	return strbuilder_build(b);
}
