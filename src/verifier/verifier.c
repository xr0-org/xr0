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
	int issplit;
	union {
		struct mux *mux;
		struct state *s;
	} u;

	int ininv;
	struct state *context;	/* state before invariant */
	char *label;		/* invariant label, may be NULL */
};

static struct verifier *
_create(struct state *s, int ininv, struct state *context, char *label)
{
	assert(s);

	struct verifier *v = malloc(sizeof(struct verifier));
	assert(v);

	v->issplit = 0;
	v->u.s = s;

	v->ininv = ininv;
	v->context = context;
	v->label = label;

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
	return _create(s, 0, NULL, NULL);
}

void
verifier_destroy(struct verifier *v)
{
	assert(verifier_atend(v));

	if (v->issplit) {
		assert(v->u.mux);
		mux_destroy(v->u.mux);
	} else {
		/*state_destroy(v->u.s);*/
	}
	/*rconst_destroy(v->rconst);*/
	free(v);
}

static int
verifier_atinvariantend(struct verifier *);

char *
verifier_str(struct verifier *v)
{
	if (v->issplit)
		return verifier_str(
			mux_firstnot(
				v->u.mux,
				v->ininv
					? verifier_atinvariantend
					: verifier_atend
			)
		);

	struct strbuilder *b = strbuilder_create();
	struct state *s = v->u.s;
	strbuilder_printf(b, "mode:\t");
	if (v->ininv) {
		strbuilder_printf(b, "INV");
		if (v->label)
			strbuilder_printf(b, " %s", v->label);
		strbuilder_printf(b, "\n");
	} else {
		strbuilder_printf(b, "EXEC\n");
	}
	strbuilder_printf(b, "\ntext:\n%s\n", state_programtext(s));
	strbuilder_printf(b, "%s\n", state_str(s));
	return strbuilder_build(b);
}

int
verifier_atend(struct verifier *v)
{
	return v->issplit
		? mux_all(v->u.mux, verifier_atend)
		: state_atend(v->u.s);
}

static int
verifier_atinvariantend(struct verifier *v)
{
	return v->issplit
		? mux_all(v->u.mux, verifier_atinvariantend)
		: state_atinvariantend(v->u.s);
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

static void
verifier_split(struct verifier *v, struct splitinstruct *inst);

struct error *
verifier_progress(struct verifier *v, progressor *prog)
{
	if (v->ininv) {
		if (verifier_atinvariantend(v)) {
			struct error *err = mux_one_verifies(
				v->u.mux, v->context
			);
			if (err) {
				return error_printf("invariant: %w", err);
			}
			/* TODO: if label present, store invariant states
			 * accessible via v->u.mux against it in map
			 * accessible to each of them, and then instruct all the
			 * verifiers in v->u.mux to leave the invariants
			 * and proceed normally. */
			assert(0);
		}

		if (v->issplit)
			return verifier_progress(
				mux_firstnot(
					v->u.mux,
					verifier_atinvariantend
				), prog
			);

		struct error *err = prog(v->u.s);
		if (err) {
			if (error_to_mustsplit(err)) {
				verifier_split(v, error_get_splitinstruct(err));
				return NULL;
			}
			assert(!error_to_enterinvariant(err));
			return err;
		}
		return NULL;
	}

	if (v->issplit) {
		assert(!mux_all(v->u.mux, verifier_atend));

		return verifier_progress(
			mux_firstnot(v->u.mux, verifier_atend), prog
		);
	}
	struct error *err = prog(v->u.s);
	if (err) {
		if (error_to_mustsplit(err)) {
			verifier_split(v, error_get_splitinstruct(err));
			return NULL;
		}
		if (error_to_enterinvariant(err)) {
			assert(!v->ininv);
			v->ininv = 1;
			v->context = state_copy(v->u.s);
			if (error_enterinvariant_haslabel(err))
				v->label = error_enterinvariant_label(err);

			return NULL;
		}
		return err;
	}
	return NULL;
}


/* verifier_split */

static struct verifier_arr *
verifier_gensplits(struct verifier *, struct splitinstruct *);

static void
verifier_split(struct verifier *v, struct splitinstruct *inst)
{
	assert(!v->issplit);
	/* TODO: destroy v->s */
	v->issplit = 1;
	v->u.mux = mux_create(verifier_gensplits(v, inst));
}

static struct verifier *
_verifier_copywithsplit(struct verifier *old, struct lsi_le *split);

static struct verifier_arr *
verifier_gensplits(struct verifier *v, struct splitinstruct *inst)
{
	struct verifier_arr *arr = verifier_arr_create();
	verifier_arr_append(
		arr, _verifier_copywithsplit(v, splitinstruct_0(inst))
	);
	verifier_arr_append(
		arr, _verifier_copywithsplit(v, splitinstruct_1(inst))
	);
	return arr;
}

static struct verifier *
_verifier_copywithsplit(struct verifier *old, struct lsi_le *split)
{
	return _create(
		state_split(old->u.s, split),
		old->ininv,
		old->context,
		old->label
	);
}

struct lexememarker *
verifier_lexememarker(struct verifier *v)
{
	if (v->ininv && verifier_atinvariantend(v))
		return NULL;

	return v->issplit
		? verifier_lexememarker(
			mux_firstnot(
				v->u.mux,
				v->ininv
					? verifier_atinvariantend
					: verifier_atend
			)
		)
		: state_lexememarker(v->u.s);
}

struct error *
verifier_verify(struct verifier *v, struct state *s)
{
	assert(v->ininv);
	return v->issplit
		? mux_one_verifies(v->u.mux, s)
		: state_constraintverify_all(v->u.s, s); 
}
