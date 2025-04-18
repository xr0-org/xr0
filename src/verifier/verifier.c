#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
#include "state.h"
#include "verifier.h"
#include "value.h"
#include "lsi.h"

#include "arr.h"
#include "mux.h"

struct verifier {
	struct mux *mux;

	struct state *context;	/* state before invariant */
	char *label;		/* invariant label, may be NULL */
	struct mux *pre_inv_root;

	struct map *inv_m;
};

static struct verifier *
_create(struct state *s)
{
	assert(s);

	struct verifier *v = malloc(sizeof(struct verifier));
	assert(v);

	v->mux = mux_create(s);

	v->context = NULL;
	v->label = NULL;
	v->pre_inv_root = NULL;

	v->inv_m = map_create();

	return v;
}

struct verifier *
verifier_create(struct ast_function *f, struct externals *ext)
{
	assert(ast_block_nstmts(ast_function_abstract(f)) == 0);

	struct state *s = state_create(
		frame_callactual_create(
			ast_function_name(f),
			ast_function_body(f),
			/* XXX */
			ast_expr_identifier_create(dynamic_str("base act")),
			f
			
		), ext
	);
	ast_function_initparams(f, s);
	return _create(s);
}

void
verifier_destroy(struct verifier *v)
{
	assert(verifier_atend(v));

	mux_destroy(v->mux);
	free(v);
}

char *
verifier_str(struct verifier *v)
{
	struct strbuilder *b = strbuilder_create();

	struct map *inv = v->inv_m;
	if (inv->n) {
		int i;

		strbuilder_printf(b, "labels: ");
		for (i = 0; i < inv->n; i++)
			strbuilder_printf(
				b,
				"%s%s",
				inv->entry[i].key,
				i+1<inv->n ? ", " : ""
			);
		strbuilder_printf(b, "\n\n");
	}
	strbuilder_printf(b, "mode:\t");
	if (mux_isactive(v->mux)) {
		struct state *s = mux_state(v->mux);
		if (state_ininvariant(s)) {
			strbuilder_printf(b, "INV");
			if (v->label)
				strbuilder_printf(b, " %s", v->label);
			strbuilder_printf(b, "\n");
		} else {
			strbuilder_printf(b, "EXEC\n");
		}
		strbuilder_printf(b, "\ntext:\n%s\n", state_programtext(s));
		strbuilder_printf(b, "%s\n", state_str(s));
	} else {
		strbuilder_printf(b, "END\n");
	}
	return strbuilder_build(b);
}

int
verifier_atend(struct verifier *v)
{
	return !v->pre_inv_root && !mux_isactive(v->mux);
}

/* verifier_progress */

progressor *
progressor_step(void)
{
	return state_step;
}

progressor *
progressor_next(void)
{
	return state_next;
}

struct error *
verifier_progress(struct verifier *v, progressor *prog)
{
	assert(!verifier_atend(v));

	if (mux_isactive(v->mux)) {
		struct mux *leaf = mux_activeleaf(v->mux);
		struct state *s = mux_state(leaf);
		struct error *err = mux_progress(leaf, prog);
		if (err) {
			if (error_to_enterinvariant(err)) {
				assert(!v->pre_inv_root);

				if (error_enterinvariant_haslabel(err)) {
					char *label = error_enterinvariant_label(err);
					assert(!map_get(v->inv_m, label));
					v->label = label;
				}
				v->context = state_copy(s);
				v->pre_inv_root = v->mux;
				v->mux = leaf;
				return NULL;
			}
			return state_stacktrace(s, err);
		}
	}
	if (v->pre_inv_root && !mux_isactive(v->mux)) {
		struct error *err = mux_one_verifies(v->mux, v->context);
		if (err) {
			return state_stacktrace(
				v->context,
				error_printf("invariant %w not satisfied", err)
			);
		}

		if (v->label)
			map_set(
				v->inv_m, dynamic_str(v->label), mux_copy(v->mux)
			);

		mux_endinvariant(v->mux);
		v->mux = v->pre_inv_root;
		v->pre_inv_root = NULL;

		v->context = NULL; /* TODO: state_destroy(v->context); */
		v->label = NULL;
	}
	return NULL;
}

struct lexememarker *
verifier_lexememarker(struct verifier *v)
{
	return verifier_atend(v) ? NULL : state_lexememarker(mux_state(v->mux));
}
