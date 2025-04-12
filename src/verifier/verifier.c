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

struct verifier;

static struct verifier *
_verifier_copywithsplit(struct verifier *old, struct lsi_le *);

static void
_verifier_split(struct verifier *, struct mux *);

static int
_verifier_issplit(struct verifier *);

static int
_verifier_ininv(struct verifier *);

static int
_verifier_inv_haslabel(struct verifier *);

char *
_verifier_inv_label(struct verifier *);

static struct state *
_verifier_inv_context(struct verifier *);

static void
_verifier_enterinvariant(struct verifier *);

static void
_verifier_setlabel(struct verifier *, char *);

static struct mux *
_verifier_mux(struct verifier *);

static struct state *
_verifier_state(struct verifier *);

static int
verifier_atinvariantend(struct verifier *v);

char *
verifier_str(struct verifier *v)
{
	if (_verifier_issplit(v))
		return verifier_str(
			mux_firstnot(
				_verifier_mux(v),
				_verifier_ininv(v)
					? verifier_atinvariantend
					: verifier_atend
			)
		);

	struct strbuilder *b = strbuilder_create();
	struct state *s = _verifier_state(v);
	strbuilder_printf(b, "mode:\t");
	if (_verifier_ininv(v)) {
		strbuilder_printf(b, "INV");
		if (_verifier_inv_haslabel(v))
			strbuilder_printf(b, " %s", _verifier_inv_label(v));
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
	return _verifier_issplit(v)
		? mux_all(_verifier_mux(v), verifier_atend)
		: state_atend(_verifier_state(v));
}

static int
verifier_atinvariantend(struct verifier *v)
{
	return _verifier_issplit(v)
		? mux_all(_verifier_mux(v), verifier_atinvariantend)
		: state_atinvariantend(_verifier_state(v));
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
	if (_verifier_ininv(v)) {
		if (verifier_atinvariantend(v)) {
			struct error *err = mux_one_verifies(
				_verifier_mux(v),
				_verifier_inv_context(v)
			);
			if (err) {
				return error_printf("invariant: %w", err);
			}
			/* TODO: if label present, store invariant states
			 * accessible via _verifier_mux(v) against it in map
			 * accessible to each of them, and then instruct all the
			 * verifiers in _verifier_mux(v) to leave the invariants
			 * and proceed normally. */
			assert(0);
		}

		if (_verifier_issplit(v))
			return verifier_progress(
				mux_firstnot(
					_verifier_mux(v),
					verifier_atinvariantend
				), prog
			);

		struct error *err = prog(_verifier_state(v));
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

	if (_verifier_issplit(v)) {
		assert(!mux_all(_verifier_mux(v), verifier_atend));

		return verifier_progress(
			mux_firstnot(_verifier_mux(v), verifier_atend), prog
		);
	}
	struct error *err = prog(_verifier_state(v));
	if (err) {
		if (error_to_mustsplit(err)) {
			verifier_split(v, error_get_splitinstruct(err));
			return NULL;
		}
		if (error_to_enterinvariant(err)) {
			_verifier_enterinvariant(v);
			if (error_enterinvariant_haslabel(err))
				_verifier_setlabel(
					v, error_enterinvariant_label(err)
				);
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
	_verifier_split(v, mux_create(verifier_gensplits(v, inst)));
}

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

struct lexememarker *
verifier_lexememarker(struct verifier *v)
{
	if (_verifier_ininv(v) && verifier_atinvariantend(v))
		return NULL;

	return _verifier_issplit(v)
		? verifier_lexememarker(
			mux_firstnot(
				_verifier_mux(v),
				_verifier_ininv(v)
					? verifier_atinvariantend
					: verifier_atend
			)
		)
		: state_lexememarker(_verifier_state(v));
}

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


static void
_verifier_split(struct verifier *v, struct mux *mux)
{
	assert(!_verifier_issplit(v));
	/* TODO: destroy v->s */
	v->issplit = 1;
	v->u.mux = mux;
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

struct error *
verifier_verify(struct verifier *v, struct state *s)
{
	assert(v->ininv);
	return v->issplit
		? mux_one_verifies(v->u.mux, s)
		: state_constraintverify_all(v->u.s, s); 
}


static int
_verifier_issplit(struct verifier *v)
{
	return v->issplit;
}

static struct mux *
_verifier_mux(struct verifier *v)
{
	assert(v->issplit);
	return v->u.mux;
}

static struct state *
_verifier_state(struct verifier *v)
{
	assert(!v->issplit);
	return v->u.s;
}

static int
_verifier_ininv(struct verifier *v)
{
	return v->ininv;
}

static int
_verifier_inv_haslabel(struct verifier *v)
{
	assert(_verifier_ininv(v));
	return v->label != NULL;
}

char *
_verifier_inv_label(struct verifier *v)
{
	assert(_verifier_ininv(v));
	return v->label;
}

static struct state *
_verifier_inv_context(struct verifier *v)
{
	assert(_verifier_ininv(v));
	return v->context;
}

static void
_verifier_enterinvariant(struct verifier *v)
{
	assert(!v->ininv);
	v->ininv = 1;
	v->context = state_copy(v->u.s);
}

static void
_verifier_setlabel(struct verifier *v, char *l)
{
	v->label = l;
}
