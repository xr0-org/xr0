#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "le.h"

struct le_arr {
	int n;
	struct le **le;
};

struct le_arr *
le_arr_create(void)
{
	return calloc(1, sizeof(struct le_arr));
}

struct le_arr *
le_arr_copy(struct le_arr *old)
{
	struct le_arr *new = le_arr_create();
	for (int i = 0; i < old->n; i++) {
		le_arr_append(new, old->le[i]);
	}
	return new;
}

void
le_arr_destroy(struct le_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		le_destroy(arr->le[i]);
	}
	free(arr);
}

void
le_arr_append(struct le_arr *arr, struct le *le)
{
	arr->le = realloc(arr->le, sizeof(struct le *)*++arr->n);
	assert(arr->le);
	arr->le[arr->n-1] = le;
}

int
le_arr_len(struct le_arr *arr)
{
	return arr->n;
}

struct le *
le_arr_get(struct le_arr *arr, int i)
{
	return arr->le[i];
}
