#include <stdlib.h>
#include <assert.h>

#include "util.h"

struct circuitbreaker {
	int n;
	void **obj;
};

struct circuitbreaker *
circuitbreaker_create(void)
{
	return calloc(1, sizeof(struct circuitbreaker));
}

struct circuitbreaker *
circuitbreaker_copy(struct circuitbreaker *old)
{
	struct circuitbreaker *new = malloc(sizeof(struct circuitbreaker));
	new->n = old->n;
	new->obj = malloc(sizeof(void *) * old->n);
	for (int i = 0; i < old->n; i++) {
		new->obj[i] = old->obj[i];
	}
	return new;
}

void
circuitbreaker_destroy(struct circuitbreaker *cb)
{
	free(cb);
}

bool
circuitbreaker_append(struct circuitbreaker *cb, void *obj)
{
	for (int i = 0; i < cb->n; i++) {
		if (cb->obj[i] == obj) {
			return false;
		}
	}
	cb->obj = realloc(cb->obj, sizeof(void *) * ++cb->n);
	assert(cb->obj);
	cb->obj[cb->n-1] = obj;
	return true;
}
