#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "verifier.h"

#include "inv_verifier.h"
#include "inv_verifier_arr.h"
#include "inv_mux.h"

struct inv_mux {
	int index;
	struct inv_verifier_arr *inv_verifiers;
};

struct inv_mux *
inv_mux_create(struct inv_verifier_arr *inv_verifiers)
{
	struct inv_mux *inv_mux = calloc(1, sizeof(struct inv_mux));
	assert(inv_mux);
	inv_mux->inv_verifiers = inv_verifiers;
	return inv_mux;
}

void
inv_mux_destroy(struct inv_mux *inv_mux)
{
	inv_verifier_arr_destroy(inv_mux->inv_verifiers);
	free(inv_mux);
}

int
inv_mux_atend(struct inv_mux *inv_mux)
{
	int n = inv_verifier_arr_n(inv_mux->inv_verifiers);
	assert(n);

	struct inv_verifier *v = inv_verifier_arr_iv(inv_mux->inv_verifiers)[inv_mux->index];
	if (inv_verifier_atend(v)) {
		if (inv_mux->index < n-1) {
			inv_mux->index++;
			return inv_mux_atend(inv_mux);
		} else {
			return 1;
		}
	}

	return 0;
}

struct inv_verifier *
inv_mux_active_inv_verifier(struct inv_mux *inv_mux)
{
	assert(!inv_mux_atend(inv_mux));

	return inv_verifier_arr_iv(inv_mux->inv_verifiers)[inv_mux->index];
}
