#include <stdlib.h>
#include <assert.h>

struct any { 
	enum type { INT, PTR } t;
	union {
		int i;
		void *p;
	} u;
};

static struct any *
_create(enum type t)
{
	struct any *a = malloc(sizeof(struct any));
	assert(a);
	a->t = t;
	return a;
}

struct any *
any_int(int i)
{
	struct any *a = _create(INT);
	a->u.i = i;
	return a;
}

struct any *
any_ptr(void *p)
{
	struct any *a = _create(PTR);
	a->u.p = p;
	return a;
}

void
any_destroy(struct any *a)
{
	free(a);
}

int
any_as_int(struct any *a)
{
	assert(a->t == INT);
	return a->u.i;
}

void *
any_as_ptr(struct any *a)
{
	assert(a->t == PTR);
	return a->u.p;
}
