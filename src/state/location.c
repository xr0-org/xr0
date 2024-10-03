#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include "ast.h"
#include "block.h"
#include "clump.h"
#include "static.h"
#include "heap.h"
#include "intern.h"
#include "location.h"
#include "object.h"
#include "stack.h"
#include "state.h"
#include "util.h"
#include "value.h"

struct location {
	enum location_type type;
	int frame;		/* LOCATION_AUTOMATIC */
	int block;
	struct offset *offset;
};

struct location *
location_create_rconst(int block, struct offset *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = LOCATION_VCONST;
	loc->block = block;
	assert(offset);
	loc->offset = offset;
	return loc;
}

struct location *
location_create_dereferencable(int block, struct offset *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = LOCATION_DEREFERENCABLE;
	loc->block = block;
	assert(offset);
	loc->offset = offset;
	return loc;
}

struct location *
location_create_static(int block, struct offset *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = LOCATION_STATIC;
	loc->block = block;
	assert(offset);
	loc->offset = offset;
	return loc;
}

struct location *
location_create_dynamic(int block, struct offset *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = LOCATION_DYNAMIC;
	loc->block = block;
	assert(offset);
	loc->offset = offset;
	return loc;
}

struct location *
location_create_automatic(int frame, int block, struct offset *offset)
{
	struct location *loc = malloc(sizeof(struct location));
	assert(loc);
	loc->type = LOCATION_AUTOMATIC;
	loc->frame = frame;
	loc->block = block;
	assert(offset);
	loc->offset = offset;
	return loc;
}

struct location *
location_permuteheap(struct location *loc, struct permutation *p)
{
	assert(loc->type == LOCATION_DYNAMIC);
	return location_create_dynamic(
		permutation_applyinverse(p, loc->block), offset_copy(loc->offset)
	);
}

static bool
shouldderiveorder(struct location *, struct circuitbreaker *, struct state *);

struct int_arr *
location_deriveorder(struct location *loc, struct circuitbreaker *cb,
		struct state *s)
{
	struct int_arr *arr = int_arr_create();
	if (shouldderiveorder(loc, cb, s)) {
		if (loc->type == LOCATION_DYNAMIC) {
			int_arr_append(arr, loc->block);
		}
		struct object_res *res = state_get(s, loc, false);
		if (object_res_iserror(res)) {
			struct error *err = object_res_as_error(res);
			if (error_to_block_observe_noobj(err)) {
				object_res_errorignore(res);
			}
		}
		if (object_res_hasobject(res)) {
			struct object *obj = object_res_as_object(res);
			int_arr_appendrange(arr, object_deriveorder(obj, cb, s));
		}
	}
	return arr;
}

static bool
shouldderiveorder(struct location *loc, struct circuitbreaker *cb,
		struct state *s)
{
	bool onheap_and_freed =
		(loc->type == LOCATION_DYNAMIC
		 && heap_blockisfreed(state_getheap(s), loc->block));

	return circuitbreaker_append(cb, loc) && !onheap_and_freed;
}

void
location_destroy(struct location *loc)
{
	if (loc->type != LOCATION_STATIC) {
		offset_destroy(loc->offset);
		free(loc);
	}
}

static bool
offset_zero(struct offset *o);

char *
location_str(struct location *loc)
{
	struct strbuilder *b = strbuilder_create();
	switch (loc->type) {
	case LOCATION_STATIC:
		strbuilder_printf(b, "static:");
		break;
	case LOCATION_AUTOMATIC:
		strbuilder_printf(b, "stack[%d]:", loc->frame);
		break;
	case LOCATION_DYNAMIC:
		strbuilder_printf(b, "heap:");
		break;
	case LOCATION_DEREFERENCABLE:
		strbuilder_printf(b, "clump:");
		break;
	default:
		assert(false);
	}
	strbuilder_printf(b, "%d", loc->block);
	if (!offset_zero(loc->offset)) {
		char *offset = offset_str(loc->offset);
		strbuilder_printf(b, "+%s", offset);
		free(offset);
	}
	return strbuilder_build(b);
}

enum location_type
location_type(struct location *loc)
{
	return loc->type;
}

int
location_block(struct location *loc)
{
	return loc->block;
}

struct offset *
location_offset(struct location *loc)
{
	return loc->offset;
}

struct location *
location_copy(struct location *loc)
{
	switch (loc->type) {
	case LOCATION_STATIC:
		return location_create_static(
			loc->block, offset_copy(loc->offset)
		);
	case LOCATION_VCONST:
		return location_create_rconst(
			loc->block, offset_copy(loc->offset)
		);
	case LOCATION_DEREFERENCABLE:
		return location_create_dereferencable(
			loc->block, offset_copy(loc->offset)
		);
	case LOCATION_AUTOMATIC:
		return location_create_automatic(
			loc->frame, loc->block, offset_copy(loc->offset)
		);
	case LOCATION_DYNAMIC:
		return location_create_dynamic(
			loc->block, offset_copy(loc->offset)
		);
	default:
		assert(false);
	}
}

void
location_setoffset(struct location *loc, struct offset *offset)
{
	loc->offset = offset;
}

bool
location_tostatic(struct location *loc, struct static_memory *sm)
{
	bool type_equal = loc->type == LOCATION_STATIC;
	struct block *b = static_memory_getblock(sm, loc->block);
	return type_equal && b;
}

bool
location_toheap(struct location *loc, struct heap *h)
{
	bool type_equal = loc->type == LOCATION_DYNAMIC;
	struct block *b = heap_getblock(h, loc->block);
	return type_equal && b;
}

bool
location_tostack(struct location *loc, struct stack *s)
{
	if (loc->type != LOCATION_AUTOMATIC) {
		return false;
	}
	bool type_equal = loc->type == LOCATION_AUTOMATIC;
	/* XXX: should probably check that frame is greater than current frame */
	struct stack *frame = stack_getframe(s, loc->frame);
	struct block *b = stack_getblock(frame, loc->block);
	return type_equal && b;
}

bool
location_toclump(struct location *loc, struct clump *c)
{
	bool type_equal = loc->type == LOCATION_DEREFERENCABLE;
	struct block *b = clump_getblock(c, loc->block);
	return type_equal && b;
}

static bool
offset_equal(struct offset *, struct offset *);

bool
location_equal(struct location *l1, struct location *l2)
{
	return l1->type == l2->type
		&& l1->block == l2->block
		&& offset_equal(l1->offset, l2->offset);
}

bool
location_referencescaller(struct location *l1, struct location *l2, struct state *s,
		struct circuitbreaker *cb)
{
	if (location_equal(l1, l2)) {
		return true;
	}

	struct block *b = state_getblock(s, l1);
	return b && block_iscaller(b) && block_references(b, l2, s, cb);
}

bool
location_references(struct location *l1, struct location *l2, struct state *s,
		struct circuitbreaker *cb)
{
	if (location_equal(l1, l2)) {
		return true;
	}

	struct block *b = state_getblock(s, l1);
	return b && block_references(b, l2, s, cb);
}

bool
location_isauto(struct location *loc)
{
	return loc->type == LOCATION_AUTOMATIC;
}

bool
location_referencesheap(struct location *l, struct state *s, struct circuitbreaker *cb)
{
	if (l->type == LOCATION_DYNAMIC) {
		if (heap_blockisfreed(state_getheap(s), l->block)) {
			return false;
		}
		return true;
	}
	struct location *l_base = location_withoutoffset(l);
	struct object_res *res = state_get(s, l_base, false);
	if (object_res_iserror(res)) {
		assert(error_to_block_observe_noobj(object_res_as_error(res)));
		object_res_errorignore(res);
	}
	location_destroy(l_base);
	return object_res_hasobject(res)
		&& object_referencesheap(object_res_as_object(res), s, cb);
}

struct location *
location_withoutoffset(struct location *old)
{
	struct location *new = location_copy(old);
	offset_destroy(new->offset);
	new->offset = offset_create(ast_expr_constant_create(0));
	return new;
}

static struct block_res *
location_auto_getblock(struct location *, struct stack *);

static struct block_res *
block_res_or_empty(struct block *);

struct block_res *
location_getblock(struct location *loc, struct static_memory *sm,
		struct rconst *v, struct stack *s, struct heap *h,
		struct clump *c)
{
	assert(s);
	switch (loc->type) {
	case LOCATION_STATIC:
		return block_res_or_empty(
			static_memory_getblock(sm, loc->block)
		);
	case LOCATION_AUTOMATIC:
		return location_auto_getblock(loc, s);	
	case LOCATION_DYNAMIC:
		return block_res_or_empty(heap_getblock(h, loc->block));
	case LOCATION_DEREFERENCABLE:
		return block_res_or_empty(clump_getblock(c, loc->block));
	default:
		assert(false);
	}
}

static struct block_res *
location_auto_getblock(struct location *loc, struct stack *s)
{
	struct stack *f = stack_getframe(s, loc->frame);
	if (!f) {
		return block_res_error_create(
			error_printf("stack frame doesn't exist")
		);
	}
	return block_res_or_empty(stack_getblock(f, loc->block));
}

static struct block_res *
block_res_or_empty(struct block *b)
{
	return b ? block_res_block_create(b) : block_res_empty_create(b);
}

struct block *
location_getstackblock(struct location *loc, struct stack *s)
{
	assert(loc->type == LOCATION_AUTOMATIC);
	return stack_getblock(s, loc->block);
}

struct error *
location_dealloc(struct location *loc, struct heap *heap)
{
	if (loc->type != LOCATION_DYNAMIC) {
		return error_printf("not heap location");
	}
	return heap_deallocblock(heap, loc->block);
}

DEFINE_RESULT_TYPE(struct location *, loc, location_destroy, loc_res, false)


struct location_arr {
	int n;
	struct location **loc;
};

struct location_arr *
location_arr_create()
{
	struct location_arr *arr = calloc(1, sizeof(struct location_arr));
	assert(arr);
	return arr;
}

void
location_arr_destroy(struct location_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		location_destroy(arr->loc[i]);
	}
	free(arr);
}

struct location **
location_arr_loc(struct location_arr *arr)
{
	return arr->loc;
}

int
location_arr_n(struct location_arr *arr)
{
	return arr->n;
}

void
location_arr_append(struct location_arr *arr, struct location *loc)
{
	arr->loc = realloc(arr->loc, sizeof(struct location *) * ++arr->n);
	assert(arr->loc);
	int n = arr->n-1;
	arr->loc[n] = loc;
}


struct offset {
	struct ast_expr *offset;

	/* may be NULL */
	char *member; 
	struct ast_type *type;
};

static struct offset *
offset_create_act(struct ast_expr *offset, char *structmember, struct ast_type *t)
{
	assert((bool) structmember == (bool) t);

	struct offset *o = malloc(sizeof(struct offset));
	o->offset = offset;
	o->member = structmember;
	o->type = t;
	return o;
}

struct offset *
offset_create(struct ast_expr *offset)
{
	return offset_create_act(offset, NULL, NULL);
}

struct offset *
offset_create_member(struct offset *o, char *member, struct ast_type *membertype)
{
	assert(member && membertype);
	return offset_create_act(
		ast_expr_copy(offset_as_expr(o)), member, membertype
	);
}

struct offset *
offset_copy(struct offset *o)
{
	if (o->member) {
		assert(o->type);
		return offset_create_act(
			ast_expr_copy(o->offset),
			dynamic_str(o->member),
			ast_type_copy(o->type)
		);
	}
	return offset_create(ast_expr_copy(o->offset));
}

void
offset_destroy(struct offset *o)
{
	ast_expr_destroy(o->offset);
	if (o->member) {
		assert(o->type);
		free(o->member);
		ast_type_destroy(o->type);
	}
	free(o);
}

char *
offset_str(struct offset *o)
{
	struct strbuilder *b = strbuilder_create();
	char *offset = ast_expr_str(o->offset);
	strbuilder_printf(b, "%s", offset);
	free(offset);
	if (o->member) {
		strbuilder_printf(b, ".%s", o->member);
	}
	return strbuilder_build(b);
}

static bool
offset_zero(struct offset *o)
{
	if (o->member) {
		return false;
	}
	struct ast_expr *zero = ast_expr_constant_create(0);
	bool eq = ast_expr_equal(o->offset, zero);
	ast_expr_destroy(zero);
	return eq;
}

static bool
offset_equal(struct offset *o1, struct offset *o2)
{
	return ast_expr_equal(o1->offset, o2->offset) &&
		( o1->member == o2->member
			|| (strcmp(o1->member, o2->member) == 0) );
}

struct ast_expr *
offset_as_expr(struct offset *o)
{
	assert(!o->member);
	return o->offset;
}

struct ast_expr *
offset_offset(struct offset *o)
{
	return o->offset;
}

char *
offset_member(struct offset *o)
{
	return o->member;
}

struct ast_type *
offset_membertype(struct offset *o)
{
	return o->type;
}
