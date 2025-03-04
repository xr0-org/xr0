#include <stdlib.h>
#include <assert.h>

#include "util.h"

#include "expr_arr.h"
#include "expr.h"
#include "tally.h"

struct expr_arr {
	int n;
	struct lsi_expr **le;
};

struct expr_arr *
expr_arr_create(void)
{
	return calloc(1, sizeof(struct expr_arr));
}

struct expr_arr *
expr_arr_copy(struct expr_arr *old)
{
	struct expr_arr *new = expr_arr_create();
	for (int i = 0; i < old->n; i++) {
		expr_arr_append(new, _lsi_expr_copy(old->le[i]));
	}
	return new;
}

void
expr_arr_destroy(struct expr_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		_lsi_expr_destroy(arr->le[i]);
	}
	free(arr);
}

void
expr_arr_append(struct expr_arr *arr, struct lsi_expr *le)
{
	arr->le = realloc(arr->le, sizeof(struct lsi_expr *)*++arr->n);
	assert(arr->le);
	arr->le[arr->n-1] = le;
}

int
expr_arr_len(struct expr_arr *arr)
{
	return arr->n;
}

struct lsi_expr *
expr_arr_get(struct expr_arr *arr, int i)
{
	return arr->le[i];
}
