#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ext.h"
#include "ast.h"
#include "state.h"
#include "object.h"
#include "util.h"
#include "value.h"
#include "lsi.h"

struct object {
	struct value *offset, *value, *size;
};

struct object *
object_create(struct value *offset, struct value *v, struct value *size)
{
	struct object *obj = malloc(sizeof(struct object));
	assert(obj);
	assert(offset && v && size);
	obj->offset = offset;
	obj->value = v;
	obj->size = size;
	return obj;
}

struct int_arr *
object_deriveorder(struct object *obj, struct circuitbreaker *cb, struct state *s)
{
	return obj->value
		? value_deriveorder(obj->value, cb, s)
		: int_arr_create();
}

struct object *
object_permuteheaplocs(struct object *old, struct permutation *p)
{
	return object_create(
		value_copy(old->offset),
		value_permuteheaplocs(old->value, p),
		value_copy(old->size)
	);
}

void
object_destroy(struct object *obj)
{
	value_destroy(obj->offset);
	/*value_destroy(obj->value);*/ /* TODO: */
	value_destroy(obj->size);
	free(obj);
}

struct object *
object_copy(struct object *old)
{
	return object_create(
		value_copy(old->offset),
		value_copy(old->value),
		value_copy(old->size)
	);
}

struct object *
object_abstractcopy(struct object *old, struct state *s)
{
	return object_create(
		value_copy(old->offset),
		value_abstractcopy(old->value, s),
		value_copy(old->size)
	);
}

char *
object_str(struct object *obj)
{
	struct strbuilder *b = strbuilder_create();
	char *offset = value_short_str(obj->offset);
	char *value = value_str(obj->value);
	char *size = value_short_str(obj->size);
	strbuilder_printf(b, "%s:%s:|%s|", offset, value, size);
	free(size);
	free(value);
	free(offset);
	return strbuilder_build(b);
}

int
object_referencesheap(struct object *obj, struct state *s,
		struct circuitbreaker *cb)
{
	if (!circuitbreaker_append(cb, obj)) {
		/* already analysed */
		return 0;
	}
	if (!object_isdef(obj)) {
		/* TODO: do we need to exclude the case of ranges on stack? */
		return 1;
	}
	struct circuitbreaker *copy = circuitbreaker_copy(cb);
	int ans = obj->value && value_referencesheap(obj->value, s, copy);
	circuitbreaker_destroy(copy);
	return ans;
}

int
object_isdef(struct object *obj)
{
	return !value_isundef(obj->value);
}

struct value *
object_as_defvalue(struct object *obj)
{
	assert(object_isdef(obj));
	return obj->value;
}

int
object_references(struct object *obj, struct location *loc, struct state *s,
		struct circuitbreaker *cb)
{
	if (!circuitbreaker_append(cb, obj)) {
		/* already handled */
		return 0;
	}

	struct circuitbreaker *copy = circuitbreaker_copy(cb);
	struct value *v = object_as_defvalue(obj);
	int ans = v ? value_references(v, loc, s, copy) : 0;
	circuitbreaker_destroy(copy);
	return ans;
}

void
object_assign(struct object *obj, struct value *val)
{
	obj->value = val;
}

struct value *
object_offset(struct object *obj)
{
	return obj->offset;
}

struct value *
object_size(struct object *obj)
{
	return obj->size;
}


int
object_hasbefore(struct object *o, struct value *offset, struct state *s)
{
	/* o->offset < offset */
	struct lsi_le *le = lsi_le_create(
		value_to_lsi_expr(o->offset, s),
		lsi_expr_sum_create(
			value_to_lsi_expr(offset, s),
			lsi_expr_const_create(-1)
		)
	);
	int ans = state_satisfies(s, le);
	lsi_le_destroy(le);
	return ans;
}

struct object *
object_upto(struct object *old, struct value *offset, struct state *s)
{
	assert(object_hasbefore(old, offset, s));
	struct lsi_expr *size = lsi_expr_sum_create(
		value_to_lsi_expr(offset, s),
		lsi_expr_product_create(
			lsi_expr_const_create(-1),
			value_to_lsi_expr(old->offset, s)
		)
	);
	struct lsi_range *r = state_range_eval(s, size);
	lsi_expr_destroy(size);
	struct object *new = object_create(
		value_copy(old->offset),
		value_copy(old->value),
		value_int_create(lsi_range_as_const(r))
	);
	lsi_range_destroy(r);
	return new;
}

int
object_hasafter(struct object *o, struct value *offset, struct state *s)
{
	/* offset < o->offset+o->size */
	struct lsi_le *le = lsi_le_create(
		value_to_lsi_expr(offset, s),
		lsi_expr_sum_create(
			/* < */
			lsi_expr_sum_create(
				value_to_lsi_expr(o->offset, s),
				value_to_lsi_expr(o->size, s)
			),
			lsi_expr_const_create(-1)
		)
	);
	int ans = state_satisfies(s, le);
	lsi_le_destroy(le);
	return ans;
}

struct object *
object_from(struct object *old, struct value *offset, struct state *s)
{
	assert(object_hasafter(old, offset, s));
	struct lsi_expr *size = lsi_expr_sum_create(
		lsi_expr_sum_create(
			value_to_lsi_expr(old->offset, s),
			value_to_lsi_expr(old->size, s)
		),
		lsi_expr_product_create(
			lsi_expr_const_create(-1),
			value_to_lsi_expr(offset, s)
		)
	);
	struct lsi_range *r = state_range_eval(s, size);
	lsi_expr_destroy(size);
	struct object *new = object_create(
		value_copy(offset),
		value_copy(old->value),
		value_int_create(lsi_range_as_const(r))
	);
	lsi_range_destroy(r);
	return new;
}

struct object *
object_at(struct object *o, struct value *offset)
{
	return object_create(
		value_copy(offset), 
		value_copy(o->value),
		value_int_create(1)
	);
}

int
object_contains(struct object *obj, struct value *offset, struct state *s)
{
	struct lsi_expr *lw = value_to_lsi_expr(obj->offset, s);
	struct lsi_expr *up = lsi_expr_sum_create(
		lsi_expr_copy(lw), value_to_lsi_expr(obj->size, s)
	);
	struct lsi_expr *of = value_to_lsi_expr(offset, s);

	struct lsi_le *l0 = lsi_le_create(lw, of);
	struct lsi_le *l1 = lsi_le_create(
		of,
		lsi_expr_sum_create(up, lsi_expr_const_create(-1)) /* < */
	);
	
	int contains = state_satisfies(s, l0) && state_satisfies(s, l1);

	lsi_le_destroy(l1);
	lsi_le_destroy(l0);

	lsi_expr_destroy(of);

	return contains;
}


static struct value *
getorcreatestruct(struct object *, struct ast_type *, struct state *);

struct object *
object_getmember(struct object *obj, struct ast_type *t, char *member,
		struct state *s)
{
	return value_struct_member(getorcreatestruct(obj, t, s), member);
}

static struct value *
getorcreatestruct(struct object *obj, struct ast_type *t, struct state *s)
{
	if (object_isdef(obj)) {
		return object_as_defvalue(obj);
	}
	struct ast_type *complete = ast_type_struct_complete(t, state_getext(s));
	a_printf(complete, "%s is incomplete\n", ast_type_str(t));
	struct value *v = value_struct_create(complete);
	object_assign(obj, v);
	return v;
}

struct ast_type *
object_getmembertype(struct object *obj, struct ast_type *t, char *member,
		struct state *s)
{
	return value_struct_membertype(
		getorcreatestruct(obj, t, s), member
	);
}

DEFINE_RESULT_TYPE(struct object *, object, object_destroy, object_res, false)
