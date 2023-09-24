#include <stdlib.h>
#include <assert.h>
#include "ast.h"
#include "location.h"
#include "value.h"
#include "util.h"

struct value {
	enum value_type type;
	union {
		struct location *loc; /* NULL is represented as NULL location */

		/* TODO: intval, floatval, charval, etc */
	};
};

struct value *
value_ptr_create(struct location *loc)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_PTR;
	v->loc = location_copy(loc);
	return v;
}

struct value *
value_virt_create(struct location *virt)
{
	struct value *v = malloc(sizeof(struct value));
	assert(v);
	v->type = VALUE_VIRTUAL;
	v->loc = virt;
	return v;
}

struct value *
value_copy(struct value *val)
{
	assert(val);
	struct location *loc_copy = location_copy(val->loc);
	if (val->type == VALUE_VIRTUAL) {
		return value_ptr_create(loc_copy);
	}
	return value_virt_create(loc_copy);
}

void
value_destroy(struct value *v)
{
	switch (v->type) {
	case VALUE_PTR:
		if (v->loc) {
			location_destroy(v->loc);
		}
		break;
	case VALUE_VIRTUAL:
		assert(v->loc);
		location_destroy(v->loc);
		break;
	default:
		assert(false);
	}
	free(v);
}

void
value_ptr_sprint(struct value *v, struct strbuilder *);

void
value_virt_sprint(struct value *v, struct strbuilder *);

char *
value_str(struct value *v)
{
	struct strbuilder *b = strbuilder_create();
	switch (v->type) {
	case VALUE_PTR:
		value_ptr_sprint(v, b);
		break;
	case VALUE_VIRTUAL:
		value_virt_sprint(v, b);
		break;
	default:
		assert(false);
	}
	return strbuilder_build(b);
}

void
value_ptr_sprint(struct value *v, struct strbuilder *b)
{
	char *s = location_str(v->loc);
	strbuilder_printf(b, "ptr:%s", s);
	free(s);
}

void
value_virt_sprint(struct value *v, struct strbuilder *b)
{
	char *s = location_str(v->loc);
	strbuilder_printf(b, "virt:%s", s);
	free(s);
}

struct location *
value_location(struct value *v)
{
	return v->loc;
}

struct location *
value_as_ptr(struct value *v)
{
	assert(v->type == VALUE_PTR);
	return v->loc;
}

struct location *
value_as_virt(struct value *v)
{
	assert(v->type == VALUE_VIRTUAL && v->loc);
	return v->loc;
}

enum value_type
value_type(struct value *v)
{
	return v->type;
}

bool
value_isdeallocand(struct value *, struct heap *);

bool
value_heap_equivalent(struct value *v1, struct value *v2, struct stack *s1, 
		struct stack *s2, struct heap *h1, struct heap *h2)
{
	/* order v1, v2 so v1 is not NULL, or return true if both are NULL */
	if (!v1) {
		if (!v2) {
			return true;
		}
		/* ⊢ v2 && !v1 */
		return value_heap_equivalent(v2, v1, s2, s1, h2, h1);
	}
	assert(v1);

	/* similar transformation to the above guarantees that v1 is deallocand
	 * or return true if both are not */
	if (!value_isdeallocand(v1, h1)) {
		if (!value_isdeallocand(v2, h2)) {
			return true;
		}
		/* ⊢ value_isdeallocand(v2) && !value_isdeallocand(v1) */
		return value_heap_equivalent(v2, v1, s2, s1, h2, h1);
	}
	assert(value_isdeallocand(v1, h1));

	if (!value_isdeallocand(v2, h2)) {
		return false;
	}

	assert(v1->type == v2->type); /* XXX: later use switch on types */ 
	return location_heap_equivalent(v1->loc, v2->loc, s1, s2, h1, h2);
}

bool
value_isdeallocand(struct value *v, struct heap *h)
{
	if (!v) {
		return false;
	}
	assert(v->type == VALUE_PTR); /* XXX */
	return location_isdeallocand(v->loc, h);
}
