#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "util.h"
#include "object.h"
#include "lsi.h"

#include "node.h"

struct o_list { struct o_node *_; };

struct o_list *
o_list_create(void)
{
	struct o_list *l = calloc(1, sizeof(struct o_list));
	assert(l);
	return l;
}

struct o_list *
o_list_copy(struct o_list *old)
{
	struct o_list *new = o_list_create();
	if (old->_) {
		new->_ = o_node_copy(old->_);	
	}
	return new;
}

void
o_list_destroy(struct o_list *l)
{
	if (l->_) {
		o_node_destroy(l->_);
	}
	free(l);
}

char *
o_list_str(struct o_list *l)
{
	assert(l->_);

	return o_node_str(l->_);
}

void
o_list_add(struct o_list *l, struct object *o)
{
	if (l->_) {
		o_node_append(l->_, o);
	} else {
		l->_ = o_node_create(o);
	}
}

struct object *
o_list_observe(struct o_list *l, int offset, struct state *s)
{
	assert(l->_);

	return o_node_observe(l->_, offset, s);
}

struct error *
o_list_mutating_shapeverify(struct o_list *l, struct o_list *l0,
		struct state *s, struct state *s0, char *id, struct ast_type *t)
{
	assert(l->_ && l0->_);

	return o_node_mutating_shapeverify(l->_, l0->_, s, s0, id, t);
}

struct lsi_varmap *
o_list_rconst_mapping(struct o_list *l, struct ast_type *t,
		struct state *s, char *id)
{
	assert(l->_);

	return o_node_rconst_mapping(l->_, t, s, id);
}
