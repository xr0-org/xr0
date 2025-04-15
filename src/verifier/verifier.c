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

	struct mux *inv;
	struct state *context;	/* state before invariant */
	char *label;		/* invariant label, may be NULL */
};

static struct verifier *
_create(struct state *s)
{
	assert(s);

	struct verifier *v = malloc(sizeof(struct verifier));
	assert(v);

	v->mux = mux_create(s);

	v->inv = NULL;
	v->context = NULL;
	v->label = NULL;

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
	return !mux_isactive(v->mux);
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

static struct error *
_progress(struct verifier *v, progressor *prog);

struct error *
verifier_progress(struct verifier *v, progressor *prog)
{
	struct error *err = _progress(v, prog);
	if (err) {
		return err;
	}
	if (v->inv && !mux_isactive(v->inv)) {
		struct error *err = mux_one_verifies(v->inv, v->context);
		if (err) {
			return state_stacktrace(
				v->context,
				error_printf("invariant %w not satisfied", err)
			);
		}
		assert(0);
	}
	return NULL;
}

static struct error *
_progress(struct verifier *v, progressor *prog)
{
	assert(!verifier_atend(v));

	struct state *s = mux_state(v->mux);
	struct error *err = prog(s);
	if (err) {
		if (error_to_mustsplit(err)) {
			struct splitinstruct *inst = error_get_splitinstruct(
				err
			);
			mux_split(
				v->mux,
				splitinstruct_0(inst),
				splitinstruct_1(inst)
			);
			return NULL;
		}
		if (error_to_enterinvariant(err)) {
			v->inv = mux_activeleaf(v->mux);
			v->context = state_copy(s);
			if (error_enterinvariant_haslabel(err))
				v->label = error_enterinvariant_label(err);
			return NULL;
		}
		return err;
	}
	return NULL;
}

struct lexememarker *
verifier_lexememarker(struct verifier *v)
{
	return verifier_atend(v) ? NULL : state_lexememarker(mux_state(v->mux));
}

/*
struct error *
verifier_verify(struct verifier *v, struct state *s)
{
	assert(v->ininv);
	return v->issplit
		? mux_one_verifies(v->u.mux, s)
		: state_constraintverify_all(v->u.s, s); 
}
*/
