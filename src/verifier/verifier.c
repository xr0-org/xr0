#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
#include "state.h"
#include "verifier.h"
#include "value.h"

#include "arr.h"
#include "instruct.h"
#include "lsi.h"
#include "mux.h"
#include "path.h"

struct verifier;

DECLARE_RESULT_TYPE(struct verifier *, verifier, v_res)

static struct verifier *
_verifier_copywithsplit(struct verifier *old, struct lsi_le *);

static void
_verifier_split(struct verifier *, struct mux *);

static int
_verifier_issplit(struct verifier *);

static struct mux *
_verifier_mux(struct verifier *);

static struct path *
_verifier_path(struct verifier *);

char *
verifier_str(struct verifier *v)
{
	return _verifier_issplit(v)
		? verifier_str(mux_activeverifier(_verifier_mux(v)))
		: path_str(_verifier_path(v));
}

bool
verifier_atend(struct verifier *v)
{
	return _verifier_issplit(v)
		? mux_atend(_verifier_mux(v))
		: path_atend(_verifier_path(v));
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
	if (_verifier_issplit(v)) {
		struct verifier *branch = mux_activeverifier(_verifier_mux(v));
		if (verifier_atend(branch)) {
			mux_next(_verifier_mux(v));
			return NULL;
		}
		return verifier_progress(branch, prog);
	}
	struct error *err = path_progress(_verifier_path(v), prog);
	if (err) {
		struct error *inst_err = error_to_verifierinstruct(err);
		if (!inst_err) {
			return err;
		}
		verifierinstruct_do(error_get_verifierinstruct(inst_err), v);
	}
	return NULL;
}


/* verifier_split */

static struct verifier_arr *
verifier_gensplits(struct verifier *, struct splitinstruct *);

void
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

struct error *
verifier_verify(struct verifier *v, struct ast_expr *expr)
{
	return _verifier_issplit(v)
		? verifier_verify(mux_activeverifier(_verifier_mux(v)), expr)
		: path_verify(_verifier_path(v), expr);
}

struct lexememarker *
verifier_lexememarker(struct verifier *v)
{
	return _verifier_issplit(v)
		? verifier_lexememarker(mux_activeverifier(_verifier_mux(v)))
		: path_lexememarker(_verifier_path(v));
}

struct verifier {
	int issplit;
	union {
		struct mux *mux;
		struct path *p;
	};
	struct rconst *rconst;
	struct ast_function *f;
	struct externals *ext;
};

static struct verifier *
_verifier_create(struct path *p, struct rconst *rconst, struct ast_function *f,
		struct externals *ext)
{
	assert(p);

	struct verifier *v = malloc(sizeof(struct verifier));
	assert(v);
	v->issplit = 0;
	v->p = p;
	v->rconst = rconst;
	v->f = ast_function_copy(f);
	v->ext = ext;
	return v;
}

static struct state *
state_abstract(struct rconst *, struct ast_function *, struct externals *);

static struct state *
state_actual(struct rconst *, struct ast_function *, struct externals *);

struct verifier *
verifier_create(struct ast_function *f, struct externals *ext)
{
	struct rconst *rconst = rconst_create();
	return _verifier_create(
		path_create(
			state_abstract(rconst, f, ext),
			state_actual(rconst, f, ext)
		),
		rconst, f, ext
	);
}

static struct frame *
frame_setup(struct ast_function *);

static struct state *
state_abstract(struct rconst *rconst, struct ast_function *f,
		struct externals *ext)
{
	struct state *s = state_create(
		frame_callabstract_create(
			ast_function_name(f),
			ast_function_abstract(f),
			/* XXX */
			ast_expr_identifier_create(dynamic_str("base abs")),
			f
		),
		rconst, ext
	);
	ast_function_initparams(f, s);
	state_pushframe(s, frame_setup(f));
	return s;
}

static struct frame *
frame_setup(struct ast_function *f)
{
	return frame_blockfindsetup_create(
		dynamic_str("findsetup"), ast_function_abstract(f)
	);
}

static struct state *
state_actual(struct rconst *rconst, struct ast_function *f,
		struct externals *ext)
{
	struct state *s = state_create(
		frame_callactual_create(
			ast_function_name(f),
			ast_function_body(f),
			/* XXX */
			ast_expr_identifier_create(dynamic_str("base act")),
			f
			
		), rconst, ext
	);
	ast_function_initparams(f, s);
	state_pushframe(s, frame_setup(f));
	return s;
}

static struct ast_function *
copy_withsplitname(struct ast_function *, struct lsi_le *split); 

static struct verifier *
_verifier_copywithsplit(struct verifier *old, struct lsi_le *split)
{
	struct rconst *rconst = rconst_split(old->rconst, split);
	struct ast_function *f = copy_withsplitname(old->f, split);
	return _verifier_create(
		path_split(old->p, rconst, ast_function_name(f)),
		rconst,
		f,
		old->ext
	);
}

static char *
split_name(char *name, struct lsi_le *split);

static struct ast_function *
copy_withsplitname(struct ast_function *old, struct lsi_le *split)
{
	struct ast_function *f = ast_function_copy(old);
	ast_function_setname(f, split_name(ast_function_name(f), split));
	return f;
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


static void
_verifier_split(struct verifier *v, struct mux *mux)
{
	assert(!_verifier_issplit(v));
	/* TODO: destroy v->s */
	v->issplit = 1;
	v->mux = mux;
}

void
verifier_destroy(struct verifier *v)
{
	assert(verifier_atend(v));

	if (v->issplit) {
		assert(v->mux);
		mux_destroy(v->mux);
	} else {
		path_destroy(v->p);
	}
	/*rconst_destroy(v->rconst);*/
	free(v);
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
	return v->mux;
}

static struct path *
_verifier_path(struct verifier *v)
{
	assert(!v->issplit);
	return v->p;
}

DEFINE_RESULT_TYPE(struct verifier *, verifier, verifier_destroy, v_res, false)
