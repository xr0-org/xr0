#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "state.h"
#include "verifier.h"

#include "arr.h"
#include "mux.h"

struct mux {
	int isleaf;
	union {
		struct state *s;
		struct {
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
mux_firstactive(struct mux *mux, atend_func atend)
{
	assert(mux_isactive(mux, atend));

	return mux->isleaf
		? mux
		: mux_firstactive(
			mux_isactive(mux->u.t.l, atend)
				? mux->u.t.l
				: mux->u.t.r,
			atend
		);
}

int
mux_isactive(struct mux *mux, atend_func atend)
{
	return mux->isleaf
		? !atend(mux->u.s)
		: mux_isactive(mux->u.t.l, atend)
			|| mux_isactive(mux->u.t.r, atend);
}

struct state *
mux_state(struct mux *mux, atend_func atend)
{
	return mux_firstactive(mux, atend)->u.s;
}

struct error *
mux_progress(struct mux *leaf, progressor *prog, atend_func atend)
{
	assert(leaf->isleaf);

	struct state *s = leaf->u.s;
	struct error *err = prog(s);
	if (err) {
		if (error_to_mustsplit(err)) {
			struct splitinstruct *inst = error_get_splitinstruct(
				err
			);
			leaf->isleaf = 0;
			leaf->u.t.l = mux_create(
				state_split(s, splitinstruct_0(inst))
			);
			leaf->u.t.r = mux_create(
				state_split(s, splitinstruct_1(inst))
			);
			return NULL;
		}
		if (error_to_goto(err)) {
			struct state *goto_s = state_copy(s);
			char *label = error_goto_label(err);
			if (!state_goto(goto_s, label))
				return error_printf("`%s' not found", label);
			/* TODO: state_destroy(leaf->u.s); */
			leaf->u.s = goto_s;
			return NULL;
		}
		return err;
	}
	return NULL;
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
