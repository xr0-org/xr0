#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "verifier.h"

#include "arr.h"
#include "mux.h"

struct mux {
	int index;
	struct verifier_arr *verifiers;
};

struct mux *
mux_create(struct verifier_arr *verifiers)
{
	struct mux *mux = calloc(1, sizeof(struct mux));
	assert(mux);
	mux->verifiers = verifiers;
	return mux;
}

void
mux_destroy(struct mux *mux)
{
	verifier_arr_destroy(mux->verifiers);
	free(mux);
}

int
mux_atend(struct mux *mux)
{
	int n = verifier_arr_n(mux->verifiers);
	assert(n);

	return mux->index == n-1
		&& verifier_atend(mux_activeverifier(mux));
}

struct verifier *
mux_activeverifier(struct mux *mux)
{
	assert(mux->index < verifier_arr_n(mux->verifiers));

	return verifier_arr_paths(mux->verifiers)[mux->index];
}

void
mux_next(struct mux *mux)
{
	assert(!mux_atend(mux));
	mux->index++;
}
