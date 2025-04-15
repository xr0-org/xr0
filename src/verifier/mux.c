#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "state.h"

#include "arr.h"
#include "mux.h"

struct mux {
	int isleaf;
	union {
		struct state *s;
		struct tree {
			struct mux *l;
			struct mux *r;
		} t;
	} u;
};

static struct mux *
_create(int isleaf)
{
	struct mux *mux = malloc(sizeof(struct mux));
	assert(mux);
	mux->isleaf = isleaf;
	return mux;
}

struct mux *
mux_create(struct state *s)
{
	struct mux *mux = _create(1);
	mux->u.s = s;
	return mux;
}

struct mux *
mux_copy(struct mux *old)
{
	if (old->isleaf)
		return mux_create(state_copy(old->u.s));

	struct mux *new = _create(0);
	new->u.t.l = mux_copy(old->u.t.l);
	new->u.t.r = mux_copy(old->u.t.r);
	return new;
}

void
mux_destroy(struct mux *mux)
{
	if (mux->isleaf) {
		/* TODO */
		/*state_destroy(mux->u.s);*/
	} else {
		mux_destroy(mux->u.t.l);
		mux_destroy(mux->u.t.r);
	}
	free(mux);
}

struct mux *
mux_activeleaf(struct mux *mux)
{
	assert(mux_isactive(mux));

	return mux->isleaf
		? mux
		: mux_activeleaf(
			mux_isactive(mux->u.t.l) ? mux->u.t.l : mux->u.t.r
		);
}

static int
_atend(struct state *);

int
mux_isactive(struct mux *mux)
{
	return mux->isleaf
		? !_atend(mux->u.s)
		: mux_isactive(mux->u.t.l) || mux_isactive(mux->u.t.r);
}

static int
_atend(struct state *s)
{
	return state_ininvariant(s) ? state_atinvariantend(s) : state_atend(s);
}

struct state *
mux_state(struct mux *mux)
{
	return mux_activeleaf(mux)->u.s;
}

void
mux_split(struct mux *mux, struct lsi_le *l, struct lsi_le *r)
{
	struct mux *leaf = mux_activeleaf(mux);
	struct state *s = leaf->u.s;
	leaf->isleaf = 0;
	leaf->u.t.l = mux_create(state_split(s, l));
	leaf->u.t.r = mux_create(state_split(s, r));
}

struct error *
mux_one_verifies(struct mux *mux, struct state *s)
{
	if (mux->isleaf)
		return state_constraintverify_all(mux->u.s, s);

	struct error *l = mux_one_verifies(mux->u.t.l, s),
		     *r = mux_one_verifies(mux->u.t.r, s);
	return l && r
		? error_printf("%w || %w", l, r)
		: NULL;
}

void
mux_endinvariant(struct mux *mux)
{
	if (mux->isleaf)
		state_endinvariant(mux->u.s);
	else {
		mux_endinvariant(mux->u.t.l);
		mux_endinvariant(mux->u.t.r);
	}
}
