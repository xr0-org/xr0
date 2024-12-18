#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "verifier.h"

#include "arr.h"

struct verifier_arr {
	int n;
	struct verifier **verifiers;
};

struct verifier_arr *
verifier_arr_create(void)
{
	struct verifier_arr *arr = calloc(1, sizeof(struct verifier_arr));
	assert(arr);
	return arr;
}

void
verifier_arr_destroy(struct verifier_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		verifier_destroy(arr->verifiers[i]);
	}
	free(arr->verifiers);
	free(arr);
}

int
verifier_arr_n(struct verifier_arr *arr)
{
	return arr->n;
}

struct verifier **
verifier_arr_paths(struct verifier_arr *arr)
{
	return arr->verifiers;
}

int
verifier_arr_append(struct verifier_arr *arr, struct verifier *p)
{
	arr->verifiers = realloc(arr->verifiers, sizeof(struct verifier_arr) * ++arr->n);
	assert(arr->verifiers);
	int loc = arr->n-1;
	arr->verifiers[loc] = p;
	return loc;
}
