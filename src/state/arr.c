#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "state.h"

struct state_arr {
	int n;
	struct state **s;
};

struct state_arr *
state_arr_create(void)
{
	struct state_arr *arr = calloc(1, sizeof(struct state_arr));
	assert(arr);
	return arr;
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
