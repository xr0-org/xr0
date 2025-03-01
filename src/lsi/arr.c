#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "lsi.h"

#include "le.h"

struct lsi_le_arr {
	int n;
	struct lsi_le **le;
};

struct lsi_le_arr *
lsi_le_arr_create(void)
{
	return calloc(1, sizeof(struct lsi_le_arr));
}

struct lsi_le_arr *
lsi_le_arr_copy(struct lsi_le_arr *old)
{
	struct lsi_le_arr *new = lsi_le_arr_create();
	for (int i = 0; i < old->n; i++) {
		lsi_le_arr_append(new, old->le[i]);
	}
	return new;
}

void
lsi_le_arr_destroy(struct lsi_le_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		lsi_le_destroy(arr->le[i]);
	}
	free(arr);
}

void
lsi_le_arr_append(struct lsi_le_arr *arr, struct lsi_le *le)
{
	arr->le = realloc(arr->le, sizeof(struct lsi_le *)*++arr->n);
	assert(arr->le);
	arr->le[arr->n-1] = le;
}

int
lsi_le_arr_len(struct lsi_le_arr *arr)
{
	return arr->n;
}

struct lsi_le *
lsi_le_arr_get(struct lsi_le_arr *arr, int i)
{
	return arr->le[i];
}
