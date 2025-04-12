#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "verifier.h"

#include "arr.h"
#include "mux.h"

struct mux { struct verifier_arr *_; };

struct mux *
mux_create(struct verifier_arr *verifiers)
{
	struct mux *mux = calloc(1, sizeof(struct mux));
	assert(mux);
	mux->_ = verifiers;
	return mux;
}

void
mux_destroy(struct mux *mux)
{
	verifier_arr_destroy(mux->_);
	free(mux);
}

static int
_index_wherenot(struct mux *, verifier_rule);

int
mux_all(struct mux *mux, verifier_rule r)
{
	return _index_wherenot(mux, r) == -1;
}

static int
_index_wherenot(struct mux *mux, verifier_rule r)
{
	int i;

	for (i = 0; i < verifier_arr_n(mux->_); i++)
		if (!r(verifier_arr_paths(mux->_)[i]))
			return i;

	return -1;
}

struct verifier *
mux_firstnot(struct mux *mux, verifier_rule r)
{
	int i = _index_wherenot(mux, r);
	assert(i != -1);
	return verifier_arr_paths(mux->_)[i];
}


struct error *
mux_one_verifies(struct mux *mux, struct state *s)
{
	int i;

	struct strbuilder *b = strbuilder_create();

	int n = verifier_arr_n(mux->_);
	for (i = 0; i < n; i++) {
		struct error *err = verifier_verify(
			verifier_arr_paths(mux->_)[i], s
		);
		if (!err) {
			return NULL;
		}
		strbuilder_printf(
			b, "%s%s", error_str(err), i+1<n? " or " : ""
		);
	}

	return error_printf(strbuilder_build(b));
}
