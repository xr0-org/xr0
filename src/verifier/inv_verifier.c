#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "verifier.h"
#include "state.h"

#include "inv_mux.h"
#include "inv_verifier.h"

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

struct error *
inv_verifier_progress(struct inv_verifier *v, progressor *prog)
{
	assert(0);
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
