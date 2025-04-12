#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "verifier.h"

#include "inv_verifier.h"
#include "inv_verifier_arr.h"

struct inv_verifier_arr {
	int n;
	struct inv_verifier **inv_verifiers;
};

struct inv_verifier_arr *
inv_verifier_arr_create(void)
{
	struct inv_verifier_arr *arr = calloc(1, sizeof(struct inv_verifier_arr));
	assert(arr);
	return arr;
}

void
inv_verifier_arr_destroy(struct inv_verifier_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		inv_verifier_destroy(arr->inv_verifiers[i]);
	}
	free(arr->inv_verifiers);
	free(arr);
}

int
inv_verifier_arr_n(struct inv_verifier_arr *arr)
{
	return arr->n;
}

struct inv_verifier **
inv_verifier_arr_iv(struct inv_verifier_arr *arr)
{
	return arr->inv_verifiers;
}

int
inv_verifier_arr_append(struct inv_verifier_arr *arr, struct inv_verifier *p)
{
	arr->inv_verifiers = realloc(arr->inv_verifiers, sizeof(struct inv_verifier_arr) * ++arr->n);
	assert(arr->inv_verifiers);
	int loc = arr->n-1;
	arr->inv_verifiers[loc] = p;
	return loc;
}
