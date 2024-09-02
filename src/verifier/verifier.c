#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
#include "object.h"
#include "state.h"
#include "verifier.h"
#include "value.h"

struct verifier_arr;

static struct verifier_arr *
verifier_arr_create();

static void
verifier_arr_destroy(struct verifier_arr *);

static int
verifier_arr_n(struct verifier_arr *);

static struct verifier **
verifier_arr_paths(struct verifier_arr *);

static int
verifier_arr_append(struct verifier_arr *, struct verifier *);


/* mux: multiplexer for splittings of verifiers */
struct mux;

static struct mux *
mux_create(struct verifier_arr *);

static void
mux_destroy(struct mux *);

static int
mux_atend(struct mux *);

static struct verifier *
mux_activeverifier(struct mux *);

static void
mux_next(struct mux *);

static struct path *
path_create(struct ast_function *);

static void
path_destroy(struct path *);

static char *
path_str(struct path *);

static int
path_atend(struct path *);

static struct error *
path_progress(struct path *, progressor *, struct externals *);

static struct path *
path_copywithsplit(struct path *, struct map *split);

static struct error *
path_verify(struct path *, struct ast_expr *);

static struct lexememarker *
path_lexememarker(struct path *);


struct verifier;

static struct verifier *
_verifier_copywithsplit(struct verifier *old, struct map *split);

static void
_verifier_split(struct verifier *, struct mux *);

static int
_verifier_issplit(struct verifier *);

static struct mux *
_verifier_mux(struct verifier *);

static struct path *
_verifier_path(struct verifier *);

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

static void
verifierinstruct_do(struct verifierinstruct *, struct verifier *);

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
		_verifier_path(p), prog, _verifier_ext(p)
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

static void
verifier_split(struct verifier *p, struct splitinstruct *inst)
{
	_verifier_split(p, mux_create(verifier_gensplits(p, inst)));
}

static struct verifier_arr *
verifier_gensplits(struct verifier *p, struct splitinstruct *inst)
{
	struct verifier_arr *arr = verifier_arr_create();;
	struct map **split = splitinstruct_splits(inst);
	int n = splitinstruct_n(inst);
	for (int i = 0; i < n; i++) {
		verifier_arr_append(arr, _verifier_copywithsplit(p, split[i]));
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
	struct externals *ext;
};

static struct verifier *
_verifier_create(struct path *s, struct rconst *rconst, struct externals *ext)
{
	assert(s);

	struct verifier *p = malloc(sizeof(struct verifier));
	assert(p);
	p->issplit = 0;
	p->s = s;
	p->ext = ext;
	p->rconst = rconst;
	return p;
}

struct verifier *
verifier_create(struct ast_function *f, struct externals *ext)
{
	return _verifier_create(path_create(f), rconst_create(), ext);
}

static struct rconst *
splitcopy(struct rconst *, struct map *split);

static struct verifier *
_verifier_copywithsplit(struct verifier *old, struct map *split)
{
	return _verifier_create(
		path_copywithsplit(old->s, split),
		splitcopy(old->rconst, split),
		old->ext
	);
}

static struct rconst *
splitcopy(struct rconst *old, struct map *split)
{
	struct rconst *new = rconst_copy(old);
	for (int i = 0; i < split->n; i++) {
		struct entry e = split->entry[i];
		struct value *v = rconst_get(new, e.key);
		assert(v);
		if (!value_splitassume(v, (struct number *) e.value)) {
			assert(false);
			return 0;
		}
	}
	return new;
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
	rconst_destroy(p->rconst);
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

static struct externals *
_verifier_ext(struct verifier *p)
{
	return p->ext;
}



struct mux {
	int index;
	struct verifier_arr *verifiers;
};

static struct mux *
mux_create(struct verifier_arr *verifiers)
{
	struct mux *mux = calloc(1, sizeof(struct mux));
	assert(mux);
	mux->verifiers = verifiers;
	return mux;
}

static void
mux_destroy(struct mux *mux)
{
	verifier_arr_destroy(mux->verifiers);
	free(mux);
}

static int
mux_atend(struct mux *mux)
{
	int n = verifier_arr_n(mux->verifiers);
	assert(n);

	return mux->index == n-1
		&& verifier_atend(mux_activeverifier(mux));
}

static struct verifier *
mux_activeverifier(struct mux *mux)
{
	assert(mux->index < verifier_arr_n(mux->verifiers));

	return verifier_arr_paths(mux->verifiers)[mux->index];
}

static void
mux_next(struct mux *mux)
{
	assert(!mux_atend(mux));
	mux->index++;
}


struct verifier_arr {
	int n;
	struct verifier **verifiers;
};

static struct verifier_arr *
verifier_arr_create()
{
	struct verifier_arr *arr = calloc(1, sizeof(struct verifier_arr));
	assert(arr);
	return arr;
}

static void
verifier_arr_destroy(struct verifier_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		verifier_destroy(arr->verifiers[i]);
	}
	free(arr->verifiers);
	free(arr);
}

static int
verifier_arr_n(struct verifier_arr *arr)
{
	return arr->n;
}

static struct verifier **
verifier_arr_paths(struct verifier_arr *arr)
{
	return arr->verifiers;
}

static int
verifier_arr_append(struct verifier_arr *arr, struct verifier *p)
{
	arr->verifiers = realloc(arr->verifiers, sizeof(struct verifier_arr) * ++arr->n);
	assert(arr->verifiers);
	int loc = arr->n-1;
	arr->verifiers[loc] = p;
	return loc;
}

struct path {
	enum path_state {
		PATH_STATE_UNINIT,
		PATH_STATE_SETUPABSTRACT,
		PATH_STATE_ABSTRACT,
		PATH_STATE_HALFWAY,
		PATH_STATE_SETUPACTUAL,
		PATH_STATE_ACTUAL,
		PATH_STATE_AUDIT,
		PATH_STATE_ATEND,
	} state;
	struct state *abstract, *actual;
	struct ast_function *f;
};

static struct path *
path_create(struct ast_function *f)
{
	struct path *s = malloc(sizeof(struct path));
	assert(s);
	s->f = ast_function_copy(f);
	s->state = PATH_STATE_UNINIT;
	return s;
}

static void
path_destroy(struct path *s)
{
	/*state_destroy(s->abstract);*/
	/*state_destroy(s->actual);*/
	ast_function_destroy(s->f);
	free(s);
}

static char *
setupabstract_str(struct path *);

static char *
abstract_str(struct path *);

static char *
setupactual_str(struct path *);

static char *
actual_str(struct path *);

static char *
path_str(struct path *s)
{
	switch (s->state) {
	case PATH_STATE_UNINIT:
		return dynamic_str("path init abstract state");
	case PATH_STATE_SETUPABSTRACT:
		return setupabstract_str(s);
	case PATH_STATE_ABSTRACT:
		return abstract_str(s);
	case PATH_STATE_HALFWAY:
		return dynamic_str("path init actual state");
	case PATH_STATE_SETUPACTUAL:
		return setupactual_str(s);
	case PATH_STATE_ACTUAL:
		return actual_str(s);
	case PATH_STATE_AUDIT:
		return dynamic_str("path audit");
	case PATH_STATE_ATEND:
		return dynamic_str("path at end");
	default:
		assert(false);
	}
}

static char *
abstract_str(struct path *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tABSTRACT\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->abstract));
	strbuilder_printf(b, "%s\n", state_str(s->abstract));
	return strbuilder_build(b);
}

static char *
actual_str(struct path *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tACTUAL\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->actual));
	strbuilder_printf(b, "%s\n", state_str(s->actual));
	return strbuilder_build(b);
}

static char *
setupabstract_str(struct path *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tSETUP (ABSTRACT)\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->abstract));
	strbuilder_printf(b, "%s\n", state_str(s->abstract));
	return strbuilder_build(b);
}

static char *
setupactual_str(struct path *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tSETUP (ACTUAL)\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(s->actual));
	strbuilder_printf(b, "%s\n", state_str(s->actual));
	return strbuilder_build(b);
}

static int
path_atend(struct path *s)
{
	return s->state == PATH_STATE_ATEND;
}


/* path_progress */

static struct error *
init_abstract(struct path *, struct externals *);

static struct error *
init_actual(struct path *, struct externals *);

static struct error *
audit(struct path *);

static struct error *
progress_setupabstract(struct path *s, progressor *);

static struct error *
progress_abstract(struct path *s, progressor *);

static struct error *
progress_setupactual(struct path *s, progressor *);

static struct error *
progress_actual(struct path *s, progressor *);

static struct error *
path_progress(struct path *s, progressor *prog, struct externals *ext)
{
	switch (s->state) {
	case PATH_STATE_UNINIT:
		return init_abstract(s, ext);
	case PATH_STATE_HALFWAY:
		return init_actual(s, ext);
	case PATH_STATE_AUDIT:
		return audit(s);
	case PATH_STATE_SETUPABSTRACT:
		return progress_setupabstract(s, prog);
	case PATH_STATE_ABSTRACT:
		return progress_abstract(s, prog);
	case PATH_STATE_SETUPACTUAL:
		return progress_setupactual(s, prog);
	case PATH_STATE_ACTUAL:
		return progress_actual(s, prog);
	case PATH_STATE_ATEND:
	default:
		assert(false);
	}
}

static struct error *
init_abstract(struct path *s, struct externals *ext)
{
	struct error *err;

	struct frame *f = frame_callabstract_create(
		ast_function_name(s->f),
		ast_function_abstract(s->f),
		ast_expr_identifier_create(dynamic_str("base abs")), /* XXX */
		s->f
	);
	s->abstract = state_create(f, ext);
	if ((err = ast_function_initparams(s->f, s->abstract))) {
		return err;
	}
	assert(!state_readregister(s->abstract));
	struct frame *setupframe = frame_blockfindsetup_create(
		dynamic_str("setup"),
		ast_function_abstract(s->f)
	);
	state_pushframe(s->abstract, setupframe);
	s->state = PATH_STATE_SETUPABSTRACT;
	return NULL;
}

static struct error *
init_actual(struct path *s, struct externals *ext)
{
	struct error *err;

	/* if body empty just apply setup */
	struct frame *f = frame_callactual_create(
		ast_function_name(s->f),
		ast_function_body(s->f),
		ast_expr_identifier_create(dynamic_str("base act")), /* XXX */
		s->f
	);
	s->actual = state_create(f, ext);
	if ((err = ast_function_initparams(s->f, s->actual))) {
		return err;
	}
	assert(!state_readregister(s->actual));
	state_setrconsts(s->actual, s->abstract);
	struct frame *setupframe = frame_blockfindsetup_create(
		dynamic_str("setup"),
		ast_function_abstract(s->f)
	);
	state_pushframe(s->actual, setupframe);
	s->state = PATH_STATE_SETUPACTUAL;
	return NULL;
}

static struct error *
audit(struct path *s)
{
	if (state_hasgarbage(s->actual)) {
		v_printf("actual: %s", state_str(s->actual));
		return error_printf(
			"%s: garbage on heap", ast_function_name(s->f)
		);
	}
	if (!state_equal(s->actual, s->abstract)) {
		/* unequal states are printed by state_equal so that the user
		 * can see the states with undeclared vars */
		return error_printf(
			"%s: actual and abstract states differ",
			ast_function_name(s->f)
		);
	}
	s->state = PATH_STATE_ATEND;
	return NULL;
}

static struct error *
progressortrace(struct state *, progressor *);

static struct error *
progress_setupabstract(struct path *s, progressor *prog)
{
	if (state_atsetupend(s->abstract)) {
		s->state = PATH_STATE_ABSTRACT;
		return NULL;
	}
	return progressortrace(s->abstract, prog);
}

static struct error *
progressortrace(struct state *s, progressor *prog)
{
	struct error *err = prog(s);
	if (err) {
		return state_stacktrace(s, err);
	}
	return NULL;
}

static struct error *
progress_abstract(struct path *s, progressor *prog)
{	
	if (state_atend(s->abstract)) {
		s->state = PATH_STATE_HALFWAY;
		return NULL;
	}
	return progressortrace(s->abstract, prog);
}

static struct error *
progress_setupactual(struct path *s, progressor *prog)
{
	if (state_atsetupend(s->actual)) {
		s->state = PATH_STATE_ACTUAL;
		return NULL;
	}
	return progressortrace(s->actual, prog);
}

static struct error *
progress_actual(struct path *s, progressor *prog)
{	
	if (state_atend(s->actual)) {
		s->state = PATH_STATE_AUDIT;
		return NULL;
	}
	return progressortrace(s->actual, prog);
}

static struct ast_function *
copy_withsplitname(struct ast_function *, struct map *split); 

static struct path *
path_copywithsplit(struct path *old, struct map *split)
{
	struct path *s = path_create(copy_withsplitname(old->f, split));
	char *fname = ast_function_name(s->f);
	s->state = old->state;
	switch (old->state) {
	case PATH_STATE_SETUPABSTRACT:
	case PATH_STATE_ABSTRACT:
		s->abstract = state_copywithname(old->abstract, fname);
		if (!state_split(s->abstract, split)) {
			s->state = PATH_STATE_ATEND;
		}
		break;
	case PATH_STATE_SETUPACTUAL:
	case PATH_STATE_ACTUAL:
		s->abstract = state_copywithname(old->abstract, fname);
		s->actual = state_copywithname(old->actual, fname);
		if (!state_split(s->actual, split)) {
			s->state = PATH_STATE_ATEND;
		}
		state_setrconsts(s->abstract, s->actual);
		break;
	default:
		assert(false);
	}
	return s;
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


static struct error *
path_verify(struct path *s, struct ast_expr *expr)
{
	switch(s->state) {
	case PATH_STATE_ABSTRACT:
		return ast_stmt_verify(
			ast_stmt_create_expr(NULL, expr), s->abstract
		);
	case PATH_STATE_ACTUAL:
		return ast_stmt_verify(
			ast_stmt_create_expr(NULL, expr), s->actual
		);
	case PATH_STATE_UNINIT:
	case PATH_STATE_HALFWAY:
	case PATH_STATE_AUDIT:
	case PATH_STATE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}

static struct lexememarker *
path_lexememarker(struct path *s)
{
	switch (s->state) {
	case PATH_STATE_SETUPABSTRACT:
	case PATH_STATE_ABSTRACT:
		return state_lexememarker(s->abstract);
	case PATH_STATE_SETUPACTUAL:
	case PATH_STATE_ACTUAL:
		return state_lexememarker(s->actual);	
	case PATH_STATE_UNINIT:
	case PATH_STATE_HALFWAY:
	case PATH_STATE_AUDIT:
	case PATH_STATE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}


struct verifierinstruct {
	enum verifierinstruct_type {
		PATHINSTRUCT_SPLIT,
	} type;
	union {
		struct splitinstruct *split;
	};
};

struct verifierinstruct *
verifierinstruct_split(struct splitinstruct *s)
{
	struct verifierinstruct *inst = malloc(sizeof(struct verifierinstruct));
	assert(inst);
	inst->type = PATHINSTRUCT_SPLIT;
	inst->split = s;
	return inst;
}

static void
verifierinstruct_do(struct verifierinstruct *inst, struct verifier *p)
{
	switch (inst->type) {
	case PATHINSTRUCT_SPLIT:
		verifier_split(p, inst->split);
		break;
	default:
		assert(false);
	}
}
