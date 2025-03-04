#include <stdlib.h>
#include <assert.h>

#include "util.h"

#include "arr.h"
#include "le.h"
#include "tally.h"

struct string_arr *
le_arr_getvars(struct le_arr *arr)
{
	int i;

	struct tally *t = tally_create();
	for (i = 0; i < le_arr_len(arr); i++) {
		struct tally *sum = tally_sum(
			t, _lsi_le_tally(le_arr_get(arr, i))
		);
		tally_destroy(t);
		t = sum;
	}
	struct string_arr *vars = tally_getvars(t);
	tally_destroy(t);

	return vars;
}

struct le_arr {
	int n;
	struct lsi_le **le;
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
		le_arr_append(new, _lsi_le_copy(old->le[i]));
	}
	return new;
}

void
le_arr_destroy(struct le_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		_lsi_le_destroy(arr->le[i]);
	}
	free(arr);
}

void
le_arr_append(struct le_arr *arr, struct lsi_le *le)
{
	arr->le = realloc(arr->le, sizeof(struct lsi_le *)*++arr->n);
	assert(arr->le);
	arr->le[arr->n-1] = le;
}

int
le_arr_len(struct le_arr *arr)
{
	return arr->n;
}

struct lsi_le *
le_arr_get(struct le_arr *arr, int i)
{
	return arr->le[i];
}
