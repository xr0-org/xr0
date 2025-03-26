#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "util.h"
#include "object.h"
#include "value.h"
#include "lsi.h"
#include "state.h"

#include "constraint.h"

#include "intern.h"
#include "node.h"

struct o_node {
	struct object *_;
	struct o_node *next;
};

static struct o_node *
_create(struct object *o, struct o_node *next)
{
	struct o_node *n = malloc(sizeof(struct o_node));
	assert(n);
	n->_ = o;
	n->next = next;
	return n;
}

struct o_node *
o_node_create(struct object *o)
{
	return _create(o, NULL);
}

struct o_node *
o_node_copy(struct o_node *o)
{
	return _create(
		object_copy(o->_),
		o->next ? o_node_copy(o->next) : NULL
	);
}

void
o_node_destroy(struct o_node *n)
{
	if (n->next) {
		o_node_destroy(n->next);
	}
	free(n);
}

char *
o_node_str(struct o_node *n)
{
	struct strbuilder *b = strbuilder_create();

	char *s = object_str(n->_);
	strbuilder_printf(b, "%s", s);
	free(s);

	if (n->next) {
		char *next = o_node_str(n->next);	
		strbuilder_printf(b, " %s", next);
		free(next);
	}

	return strbuilder_build(b);
}

void
o_node_append(struct o_node *n, struct object *o)
{
	if (n->next) {
		o_node_append(n->next, o);
	} else {
		n->next = o_node_create(o);
	}
}

struct object *
o_node_observe(struct o_node *n, int offset, struct state *s)
{
	struct value *v_lw = value_int_create(offset),
		     *v_up = value_int_create(offset+1);

	if (!object_contains(n->_, v_lw, s)) {
		assert(n->next);
		return o_node_observe(n->next, offset, s);
	}

	struct object *obj = n->_;

	if (object_hasbefore(obj, v_lw, s)) {
		n->_ = object_upto(obj, v_lw, s);
		n->next = _create(object_from(obj, v_lw, s), n->next);
		return o_node_observe(n->next, offset, s);
	}

	n->_ = object_at(obj, v_lw);

	if (object_hasafter(obj, v_up, s)) {
		n->next = _create(object_from(obj, v_up, s), n->next);
	}

	return n->_;
}

static char *
_deref_or_access_str(struct object *obj, char *block);

struct lsi_varmap *
o_node_rconst_mapping(struct o_node *n, struct ast_type *t, struct state *s,
		char *id)
{
	struct lsi_varmap *lv = lsi_varmap_create();

	if (n->next) {
		lsi_varmap_addrange(
			lv,
			o_node_rconst_mapping(n->next, t, s, id)
		);
	}

	struct object *obj = n->_;
	char *alias = _deref_or_access_str(obj, id);
	if (object_isdef(obj)) {
		lsi_varmap_addrange(
			lv,
			value_rconst_mapping(
				object_as_defvalue(n->_),
				ast_type_deref(t),
				s, alias
			)
		);
	}
	free(alias);
	return lv;
}

static struct error *
_modulate(struct o_node *n0, struct o_node *n1, struct state *s0,
		struct state *s1);

static struct error *
_shapeverify(struct o_node *spec_n, struct o_node *impl_n, struct state *spec,
		struct state *impl, char *id, struct ast_type *t);

struct error *
o_node_mutating_shapeverify(struct o_node *spec_n, struct o_node *impl_n,
		struct state *spec, struct state *impl, char *id,
		struct ast_type *t)
{
	struct error *err = _modulate(spec_n, impl_n, spec, impl);
	if (err) {
		return err;
	}
	return _shapeverify(spec_n, impl_n, spec, impl, id, t);
}

static int
_const_eval(struct value *, struct state *);

static struct error *
_modulate(struct o_node *n0, struct o_node *n1, struct state *s0,
		struct state *s1)
{
	struct object *obj0 = n0->_,
		      *obj1 = n1->_;
	
	int offset0 = value_as_constant(object_offset(obj0)),
	    offset1 = value_as_constant(object_offset(obj1));

	/* an invariant that holds because each successive iteration
	 * proceeds from a point where corresponding objects have been
	 * cut to end within the blocks, at the same offset */
	assert(offset0 == offset1);

	int size0 = _const_eval(object_size(obj0), s0),
	    size1 = _const_eval(object_size(obj1), s1);

	if (size0 < size1) {
		return _modulate(n1, n0, s1, s0);	
	} 
	/* |- size0 >= size1 */
		
	if (size0 > size1) {
		struct value *end1 = value_int_create(
			_const_eval(object_offset(obj1), s1) + size1
		);

		n0->_ = object_upto(obj0, end1, s0);
		n0->next = _create(object_from(obj0, end1, s0), n0->next);

		value_destroy(end1);
	}

	assert(
		_const_eval(object_size(n0->_), s0)
		==
		_const_eval(object_size(n1->_), s1)
	);

	if (!n0->next) {
		assert(!n1->next);
		return NULL;
	}
	assert(n0->next && n1->next); /* blocks of same size */

	return _modulate(n0->next, n1->next, s0, s1);
}

static struct lsi_range *
_value_range_eval(struct value *, struct state *);

static int
_const_eval(struct value *v, struct state *s)
{
	struct lsi_range *r = _value_range_eval(v, s);
	int c = lsi_range_as_const(r);
	lsi_range_destroy(r);
	return c;
}

static struct lsi_expr *
_value_to_lsi_expr(struct value *, struct state *);

static struct lsi_range *
_value_range_eval(struct value *v, struct state *s)
{
	struct lsi_expr *e = _value_to_lsi_expr(v, s);
	struct lsi_range *r = state_range_eval(s, e);
	lsi_expr_destroy(e);
	return r;
}

static struct lsi_expr *
_value_to_lsi_expr(struct value *v, struct state *s)
{
	return value_isconstant(v)
		? lsi_expr_const_create(value_as_constant(v))
		: lsi_expr_var_create(value_to_rconstid(v, s));
}

static struct error *
_shapeverify(struct o_node *spec_n, struct o_node *impl_n,
		struct state *spec, struct state *impl, char *id,
		struct ast_type *t)
{
	struct object *spec_o = spec_n->_,
		      *impl_o = impl_n->_;

	struct constraint *c = constraint_create(
		_deref_or_access_str(spec_o, id), ast_type_copy(t), spec, impl
	);
	struct error *err = constraint_shapeverify(c, spec_o, impl_o);
	constraint_destroy(c);
	if (err) {
		return err;
	}
	
	if (spec_n->next) {
		assert(impl_n->next);
		return _shapeverify(
			spec_n->next, impl_n->next, spec, impl, id, t
		);
	}

	return NULL;
}

static char *
_deref_or_access_str(struct object *obj, char *block)
{
	struct strbuilder *b = strbuilder_create();

	struct value *lw = object_offset(obj);
	if (value_isconstant(lw) && value_as_constant(lw) == 0) {
		strbuilder_printf(b, "*(%s)", block);
	} else {
		char *s = value_short_str(lw);
		strbuilder_printf(b, "%s[%s]", block, s);
		free(s);
	}

	return strbuilder_build(b);
}
