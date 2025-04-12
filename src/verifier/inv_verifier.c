#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "state.h"
#include "lsi.h"
#include "verifier.h"

#include "inv_mux.h"
#include "inv_verifier.h"
#include "inv_verifier_arr.h"

struct inv_verifier {
	int issplit;
	union {
		struct inv_mux *mux;
		struct state *s;
	} u;
};

struct inv_verifier *
inv_verifier_create(struct state *s)
{
	struct inv_verifier *v = calloc(1, sizeof(struct inv_verifier));
	assert(v);
	v->u.s = s;
	return v;
}

void
inv_verifier_destroy(struct inv_verifier *v)
{
	if (v->issplit)
		inv_mux_destroy(v->u.mux);
	else
		state_destroy(v->u.s);
	free(v);
}

char *
inv_verifier_str(struct inv_verifier *v)
{
	if (v->issplit)
		return inv_verifier_str(inv_mux_active_inv_verifier(v->u.mux));

	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "\ntext:\n%s\n", state_programtext(v->u.s));
	strbuilder_printf(b, "%s\n", state_str(v->u.s));
	return strbuilder_build(b);
}

static void
inv_verifier_split(struct inv_verifier *, struct splitinstruct *);

struct error *
inv_verifier_progress(struct inv_verifier *v, progressor *prog)
{
	if (v->issplit) {
		assert(!inv_mux_atend(v->u.mux));
		return inv_verifier_progress(
			inv_mux_active_inv_verifier(v->u.mux), prog
		);
	}
	struct error *err = prog(v->u.s);
	if (err) {
		if (!error_to_mustsplit(err)) {
			return err;
		}
		inv_verifier_split(v, error_get_splitinstruct(err));
	}
	return NULL;
}


/* inv_verifier_split */

static struct inv_verifier_arr *
inv_verifier_gensplits(struct inv_verifier *, struct splitinstruct *);

static void
inv_verifier_split(struct inv_verifier *v, struct splitinstruct *inst)
{
	assert(!v->issplit);
	v->issplit = 1;
	v->u.mux = inv_mux_create(inv_verifier_gensplits(v, inst));
}

static struct inv_verifier *
inv_verifier_copywithsplit(struct inv_verifier *, struct lsi_le *split);

static struct inv_verifier_arr *
inv_verifier_gensplits(struct inv_verifier *v, struct splitinstruct *inst)
{
	struct inv_verifier_arr *arr = inv_verifier_arr_create();
	inv_verifier_arr_append(
		arr, inv_verifier_copywithsplit(v, splitinstruct_0(inst))
	);
	inv_verifier_arr_append(
		arr, inv_verifier_copywithsplit(v, splitinstruct_1(inst))
	);
	return arr;
}

static char *
split_name(char *name, struct lsi_le *split);

static struct inv_verifier *
inv_verifier_copywithsplit(struct inv_verifier *v, struct lsi_le *split)
{
	return inv_verifier_create(
		state_split(
			v->u.s,
			rconst_split(state_rconstxxx(v->u.s), split),
			split_name(state_baseframename(v->u.s), split)
		)
	);
}

static char *
split_name(char *name, struct lsi_le *split)
{
	struct strbuilder *b = strbuilder_create();
	char *s = lsi_le_str(split);
	strbuilder_printf(b, "%s | %s", name, s);
	free(s);
	return strbuilder_build(b);
}

int
inv_verifier_atend(struct inv_verifier *v)
{
	return v->issplit
		? inv_mux_atend(v->u.mux)
		: state_atinvariantend(v->u.s);
}

struct lexememarker *
inv_verifier_lexememarker(struct inv_verifier *v)
{
	return v->issplit
		? inv_verifier_lexememarker(inv_mux_active_inv_verifier(v->u.mux))
		: state_lexememarker(v->u.s);
}

struct error *
inv_verifier_verify(struct inv_verifier *v, struct state *s)
{
	/* TODO: return error as disjunction of all unsatisfied conditions */
	return v->issplit
		? inv_mux_one_verifies(v->u.mux, s)
		: state_constraintverify_all(v->u.s, s);
}
