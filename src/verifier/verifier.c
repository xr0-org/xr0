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
#include "mux.h"
#include "path.h"

struct verifier;

DECLARE_RESULT_TYPE(struct verifier *, verifier, v_res)

static struct v_res *
_verifier_copywithsplit(struct verifier *old, struct map *split);

static void
_verifier_split(struct verifier *, struct mux *);

static int
_verifier_issplit(struct verifier *);

static struct mux *
_verifier_mux(struct verifier *);

static struct path *
_verifier_path(struct verifier *);

static struct rconst *
_verifier_rconst(struct verifier *);

static struct ast_function *
_verifier_function(struct verifier *);

static struct externals *
_verifier_ext(struct verifier *);


char *
verifier_str(struct verifier *p)
{
	return _verifier_issplit(p)
		? verifier_str(mux_activeverifier(_verifier_mux(p)))
		: path_str(_verifier_path(p));
}

bool
verifier_atend(struct verifier *p)
{
	return _verifier_issplit(p)
		? mux_atend(_verifier_mux(p))
		: path_atend(_verifier_path(p));
}

/* verifier_progress */

progressor *
progressor_step()
{
	return state_step;
}

progressor *
progressor_next()
{
	return state_next;
}

struct error *
verifier_progress(struct verifier *p, progressor *prog)
{
	if (_verifier_issplit(p)) {
		struct verifier *branch = mux_activeverifier(_verifier_mux(p));
		if (verifier_atend(branch)) {
			mux_next(_verifier_mux(p));
			return NULL;
		}
		return verifier_progress(branch, prog);
	}
	struct error *err = path_progress(
		_verifier_path(p), _verifier_rconst(p), _verifier_function(p),
		_verifier_ext(p), prog
	);
	if (err) {
		struct error *inst_err = error_to_verifierinstruct(err);
		if (!inst_err) {
			return err;
		}
		verifierinstruct_do(error_get_verifierinstruct(inst_err), p);
	}
	return NULL;
}


/* verifier_split */

static struct verifier_arr *
verifier_gensplits(struct verifier *, struct splitinstruct *);

void
verifier_split(struct verifier *p, struct splitinstruct *inst)
{
	_verifier_split(p, mux_create(verifier_gensplits(p, inst)));
}

static struct verifier_arr *
verifier_gensplits(struct verifier *p, struct splitinstruct *inst)
{
	struct verifier_arr *arr = verifier_arr_create();
	struct map **split = splitinstruct_splits(inst);
	int n = splitinstruct_n(inst);
	for (int i = 0; i < n; i++) {
		struct v_res *res = _verifier_copywithsplit(p, split[i]);
		if (v_res_iserror(res)) {
			/* contradictions not added to mux */
			assert(
				error_to_verifiercontradiction(
					v_res_as_error(res)
				)
			);
			continue;
		}
		verifier_arr_append(arr, v_res_as_verifier(res));
	}
	return arr;
}

struct error *
verifier_verify(struct verifier *p, struct ast_expr *expr)
{
	return _verifier_issplit(p)
		? verifier_verify(mux_activeverifier(_verifier_mux(p)), expr)
		: path_verify(_verifier_path(p), expr);
}

struct lexememarker *
verifier_lexememarker(struct verifier *p)
{
	return _verifier_issplit(p)
		? verifier_lexememarker(mux_activeverifier(_verifier_mux(p)))
		: path_lexememarker(_verifier_path(p));
}

struct verifier {
	int issplit;
	union {
		struct mux *mux;
		struct path *s;
	};
	struct rconst *rconst;
	struct ast_function *f;
	struct externals *ext;
};

static struct verifier *
_verifier_create(struct path *s, struct rconst *rconst, struct ast_function *f,
		struct externals *ext)
{
	assert(s);

	struct verifier *p = malloc(sizeof(struct verifier));
	assert(p);
	p->issplit = 0;
	p->s = s;
	p->rconst = rconst;
	p->f = ast_function_copy(f);
	p->ext = ext;
	return p;
}

struct verifier *
verifier_create(struct ast_function *f, struct externals *ext)
{
	struct rconst *rconst = rconst_create();
	return _verifier_create(path_create(rconst, f, ext), rconst, f, ext);
}

static struct rconst *
rconst_split(struct rconst *, struct map *split);

static struct ast_function *
copy_withsplitname(struct ast_function *, struct map *split); 

static struct v_res *
_verifier_copywithsplit(struct verifier *old, struct map *split)
{
	struct rconst *rconst = rconst_split(old->rconst, split);
	if (!rconst) {
		return v_res_error_create(error_verifiercontradiction());
	}
	struct ast_function *f = copy_withsplitname(old->f, split);
	return v_res_verifier_create(
		_verifier_create(
			path_copywithsplit(old->s, rconst, ast_function_name(f)),
			rconst,
			f,
			old->ext
		)
	);
}

static struct rconst *
rconst_split(struct rconst *old, struct map *split)
{
	struct rconst *new = rconst_copy(old);
	for (int i = 0; i < split->n; i++) {
		struct entry e = split->entry[i];
		struct value *v = rconst_get(new, e.key);
		assert(v);
		if (!value_splitassume(v, (struct number *) e.value)) {
			return NULL;
		}
	}
	return new;
}

static char *
split_name(char *name, struct map *split);

static struct ast_function *
copy_withsplitname(struct ast_function *old, struct map *split)
{
	struct ast_function *f = ast_function_copy(old);
	ast_function_setname(f, split_name(ast_function_name(f), split));
	return f;
}

static char *
split_name(char *name, struct map *split)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s | ", name);
	if (split->n > 1) {
		strbuilder_printf(b, "{ ");
	}
	for (int i = 0; i < split->n; i++) {
		struct entry e = split->entry[i];
		char *rconst = e.key;
		char *num = number_str((struct number *) e.value);
		strbuilder_printf(
			b, "%s ∈ %s%s", rconst, num,
			(i+1 < split->n ? "," : "")
		);
		free(num);
	}
	if (split->n > 1) {
		strbuilder_printf(b, " }");
	}
	return strbuilder_build(b);
}



static void
_verifier_split(struct verifier *p, struct mux *mux)
{
	assert(!_verifier_issplit(p));
	/* TODO: destroy p->s */
	p->issplit = 1;
	p->mux = mux;
}

void
verifier_destroy(struct verifier *p)
{
	assert(verifier_atend(p));

	if (p->issplit) {
		assert(p->mux);
		mux_destroy(p->mux);
	} else {
		path_destroy(p->s);
	}
	/*rconst_destroy(p->rconst);*/
	free(p);
}

static int
_verifier_issplit(struct verifier *p)
{
	return p->issplit;
}

static struct mux *
_verifier_mux(struct verifier *p)
{
	assert(p->issplit);
	return p->mux;
}

static struct path *
_verifier_path(struct verifier *p)
{
	assert(!p->issplit);
	return p->s;
}

static struct rconst *
_verifier_rconst(struct verifier *p)
{
	return p->rconst;
}

static struct ast_function *
_verifier_function(struct verifier *p)
{
	return p->f;
}

static struct externals *
_verifier_ext(struct verifier *p)
{
	return p->ext;
}

DEFINE_RESULT_TYPE(struct verifier *, verifier, verifier_destroy, v_res, false)
