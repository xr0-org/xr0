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

	struct verifier *v = verifier_arr_paths(mux->verifiers)[mux->index];
	if (verifier_atend(v)) {
		if (mux->index < n-1) {
			mux->index++;
			return mux_atend(mux);
		} else {
			return 1;
		}
	}

	return 0;
}

struct verifier *
mux_activeverifier(struct mux *mux)
{
	assert(!mux_atend(mux));

	return verifier_arr_paths(mux->verifiers)[mux->index];
}
