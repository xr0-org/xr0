#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "object.h"
#include "state.h"
#include "util.h"
#include "value.h"
#include "verifier.h"
#include "lsi.h"

#include "block.h"
#include "constraint.h"
#include "heap.h"
#include "location.h"
#include "stack.h"

#include "list.h"

struct block {
	bool caller;
	struct value *size;
	struct o_list *list;
};

static struct block *
_create(struct value *size, bool caller)
{
	struct block *b = malloc(sizeof(struct block));
	assert(b);
	b->caller = caller;
	b->size = size;
	b->list = o_list_create();
	o_list_add(
		b->list,
		object_create(
			value_int_create(0),
			value_undef_create(),
			value_copy(size)
		)
	);
	return b;
}

struct block *
block_create(struct value *size)
{
	return _create(size, false);
}

struct block *
block_callercreate(struct value *size)
{
	return _create(size, true);
}

void
block_destroy(struct block *b)
{
	value_destroy(b->size);
	o_list_destroy(b->list);
	free(b);
}

struct block *
block_copy(struct block *old)
{
	struct block *new = malloc(sizeof(struct block));
	assert(new);
	new->list = o_list_copy(old->list);
	new->caller = old->caller;
	new->size = value_copy(old->size);
	return new;
}

struct block *
block_permuteheaplocs(struct block *old, struct permutation *p)
{
	assert(0);
}

char *
block_str(struct block *block)
{
	struct strbuilder *b = strbuilder_create();
	char *size = value_short_str(block->size);
	char *list = o_list_str(block->list);
	strbuilder_printf(b, "|%s| %s", size, list);
	free(list);
	free(size);
	return strbuilder_build(b);
}

DECLARE_RESULT_TYPE(int, int, int_res)

static struct int_res *
_bounds_safe_offset(struct block *, struct ast_expr *, struct state *);

struct object_res *
block_observe(struct block *b, struct ast_expr *offset, struct state *s)
{
	struct int_res *res = _bounds_safe_offset(b, offset, s);
	if (int_res_iserror(res)) {
		return object_res_error_create(int_res_as_error(res));
	}
	return object_res_object_create(
		o_list_observe(b->list, int_res_as_int(res), s)
	);
}

static struct int_res *
_bounds_safe_offset(struct block *b, struct ast_expr *offset, struct state *s)
{
	struct lsi_expr *lsi_o = ast_expr_to_lsi_expr(offset);

	struct lsi_le *lw = lsi_le_create(
		lsi_expr_const_create(0), lsi_expr_copy(lsi_o)
	);
	struct lsi_le *up = lsi_le_create(
		/* non-inclusive upper bound */
		lsi_expr_copy(lsi_o),
		lsi_expr_sum_create(
			value_to_lsi_expr(b->size, s),
			lsi_expr_const_create(-1)
		)
	);
	if (!state_satisfies(s, lw) || !state_satisfies(s, up)) {
		return int_res_error_create(error_printf("out of bounds"));
	}
	lsi_le_destroy(lw);
	lsi_le_destroy(up);

	struct lsi_range *r = state_range_eval(s, lsi_o);
	if (!lsi_range_isconst(r)) {
		/* split b/w case when equal to lower bound and not */
		struct lsi_le *le = lsi_range_expr_le_lw(r, lsi_o);  
		struct lsi_le *le_neg = lsi_le_negate(le);
		return int_res_error_create(
			error_mustsplit(splitinstruct_create(le, le_neg))
		);
	}

	int offset_c = lsi_range_as_const(r);

	lsi_range_destroy(r);
	lsi_expr_destroy(lsi_o);

	return int_res_int_create(offset_c);
}

static void
nulldestruct(int x) { /* do nothing */ }

DEFINE_RESULT_TYPE(int, int, nulldestruct, int_res, true)


bool
block_references(struct block *b, struct location *loc, struct state *s,
		struct circuitbreaker *cb)
{
	assert(0);
	return o_list_references(b->list, loc, s, cb);
}

bool
block_iscaller(struct block *b)
{
	return b->caller;
}

void
block_undeclare(struct block *b, struct state *s)
{
	assert(0);
}

static struct error *
_mutating_shapeverify(struct block *spec_b, struct block *impl_b,
		struct state *spec, struct state *impl, char *id,
		struct ast_type *t);

struct error *
block_shapeverify(struct block *spec_b, struct block *impl_b,
		struct state *spec, struct state *impl, char *id,
		struct ast_type *t)
{
	spec_b = block_copy(spec_b);
	impl_b = block_copy(impl_b);
	struct error *err = _mutating_shapeverify(
		spec_b, impl_b, spec, impl, id, t
	);
	block_destroy(impl_b);
	block_destroy(spec_b);
	return err;
}

static struct error *
_constrainspec(struct block *spec_b, struct block *impl_b, struct state *spec,
		struct state *impl);

static struct error *
_mutating_shapeverify(struct block *spec_b, struct block *impl_b,
		struct state *spec, struct state *impl, char *id,
		struct ast_type *t)
{
	struct error *err = _constrainspec(spec_b, impl_b, spec, impl);
	assert(!err);
	return o_list_mutating_shapeverify(
		spec_b->list, impl_b->list, spec, impl, id, t
	);
}

static struct error *
_constraineq(struct lsi_expr *, struct lsi_expr *, struct state *);

static struct error *
_constrainspec(struct block *spec_b, struct block *impl_b, struct state *spec,
		struct state *impl)
{
	struct lsi_range *r = state_range_eval(
		impl, value_to_lsi_expr(impl_b->size, impl)
	);
	struct lsi_expr *c = lsi_expr_const_create(lsi_range_as_const(r));
	lsi_range_destroy(r);
	struct lsi_expr *spec_size = value_to_lsi_expr(spec_b->size, spec);
	struct error *err = _constraineq(spec_size, c, spec);
	lsi_expr_destroy(spec_size);
	lsi_expr_destroy(c);
	return err;
}

static struct error *
_constraineq(struct lsi_expr *e0, struct lsi_expr *e1, struct state *s)
{
	struct error *err = state_addconstraint(
		s, lsi_le_create(lsi_expr_copy(e0), lsi_expr_copy(e1))
	);
	if (err) {
		return err;
	}
	return state_addconstraint(
		s, lsi_le_create(lsi_expr_copy(e1), lsi_expr_copy(e0))
	);
}

static char *
_sizeof(char *id);

struct lsi_varmap *
block_rconst_mapping(struct block *b, struct ast_type *t, struct state *s,
		char *id)
{
	struct lsi_varmap *lv = lsi_varmap_create();

	lsi_varmap_set(lv, _sizeof(id), value_to_rconstid(b->size, s));

	lsi_varmap_addrange(lv, o_list_rconst_mapping(b->list, t, s, id));
	return lv;
}

static char *
_sizeof(char *id)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "sizeof(%s)", id);
	return strbuilder_build(b);
}


struct block_arr {
	int n;
	struct block **block;
};

struct block_arr *
block_arr_create(void)
{
	struct block_arr *arr = calloc(1, sizeof(struct block_arr));
	assert(arr);
	return arr;
}

void
block_arr_destroy(struct block_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		block_destroy(arr->block[i]);
	}
	free(arr->block);
	free(arr);
}

struct block_arr *
block_arr_copy(struct block_arr *old)
{
	struct block_arr *new = block_arr_create();
	for (int i = 0; i < old->n; i++) {
		block_arr_append(new, block_copy(old->block[i]));
	}
	return new;
}

struct block **
block_arr_blocks(struct block_arr *arr)
{
	return arr->block;
}

int
block_arr_nblocks(struct block_arr *arr)
{
	return arr->n;
}

/* block_arr_append: Append struct block to array and return index (address). */
int
block_arr_append(struct block_arr *arr, struct block *b)
{
	arr->block = realloc(arr->block, sizeof(struct block_arr) * ++arr->n);
	assert(arr->block);
	int loc = arr->n-1;
	arr->block[loc] = b;
	return loc;
}

void
block_arr_delete(struct block_arr *arr, int address)
{
	assert(false);
}

DEFINE_RESULT_TYPE(struct block *, block, block_destroy, block_res, false)

struct permutation {
	struct int_arr *arr;
};

struct permutation *
permutation_create(struct int_arr *arr)
{
	struct permutation *p = malloc(sizeof(struct permutation));
	p->arr = arr;
	return p;
}

struct permutation *
permutation_copy(struct permutation *old)
{
	struct permutation *new = malloc(sizeof(struct permutation));
	new->arr = int_arr_create();
	int *old_arr = int_arr_arr(old->arr);
	int old_len = int_arr_len(old->arr);
	for (int i = 0; i < old_len; i++) {
		int_arr_append(new->arr, old_arr[i]);
	}
	return new;
}

void
permutation_destroy(struct permutation *p)
{
	int_arr_destroy(p->arr);
	free(p);
}

int
permutation_apply(struct permutation *p, int i)
{
	assert(i < int_arr_len(p->arr));
	return int_arr_arr(p->arr)[i];
}

int
permutation_applyinverse(struct permutation *p, int i)
{
	int len = int_arr_len(p->arr);
	int *perm = int_arr_arr(p->arr);
	assert(i < len);
	for (int j = 0; j < len; j++) {
		if (perm[j] == i) {
			return j;
		}
	}
	assert(false);
}

char *
permutation_str(struct permutation *p)
{
	int len = int_arr_len(p->arr);
	int *arr = int_arr_arr(p->arr);

	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "[");
	for (int i = 0; i < len; i++) {
		strbuilder_printf(b, "%d->%d%s", i, arr[i], i+1<len ? " " : "");
	}
	strbuilder_printf(b, "]");
	return strbuilder_build(b);
}
