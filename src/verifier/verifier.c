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

struct scenario {
	enum scenario_state {
		SCENARIO_STATE_UNINIT,
		SCENARIO_STATE_SETUPABSTRACT,
		SCENARIO_STATE_ABSTRACT,
		SCENARIO_STATE_HALFWAY,
		SCENARIO_STATE_SETUPACTUAL,
		SCENARIO_STATE_ACTUAL,
		SCENARIO_STATE_AUDIT,
		SCENARIO_STATE_ATEND,
	} state;
	struct state *abstract, *actual;
	struct ast_function *f;
	struct externals *ext;
};

static struct scenario *
scenario_create(struct ast_function *, struct externals *);

static void
scenario_destroy(struct scenario *);

static struct scenario *
scenario_create(struct ast_function *f, struct externals *ext)
{
	struct scenario *s = malloc(sizeof(struct scenario));
	assert(s);
	s->f = ast_function_copy(f);
	s->ext = ext;
	s->state = SCENARIO_STATE_UNINIT;
	return s;
}

static void
scenario_destroy(struct scenario *s)
{
	/*state_destroy(s->abstract);*/
	/*state_destroy(s->actual);*/
	ast_function_destroy(s->f);
	free(s);

}


struct verifier {
	bool issplit;
	struct mux *mux;
	struct scenario *s;
};

struct verifier *
verifier_create(struct ast_function *f, struct externals *ext)
{
	struct verifier *p = calloc(1, sizeof(struct verifier));
	assert(p);
	p->s = scenario_create(f, ext);
	return p;
}

void
verifier_destroy(struct verifier *p)
{
	assert(verifier_atend(p));

	if (p->issplit) {
		assert(p->mux);
		mux_destroy(p->mux);
	} else {
		assert(p->s);
		scenario_destroy(p->s);
	}
	free(p);
}

static char *
verifier_setupabstract_str(struct verifier *);

static char *
verifier_abstract_str(struct verifier *);

static char *
verifier_setupactual_str(struct verifier *);

static char *
verifier_actual_str(struct verifier *);

char *
verifier_str(struct verifier *p)
{
	if (p->issplit) {
		return verifier_str(mux_activeverifier(p->mux));
	}

	switch (p->s->state) {
	case SCENARIO_STATE_UNINIT:
		return dynamic_str("path init abstract state");
	case SCENARIO_STATE_SETUPABSTRACT:
		return verifier_setupabstract_str(p);
	case SCENARIO_STATE_ABSTRACT:
		return verifier_abstract_str(p);
	case SCENARIO_STATE_HALFWAY:
		return dynamic_str("path init actual state");
	case SCENARIO_STATE_SETUPACTUAL:
		return verifier_setupactual_str(p);
	case SCENARIO_STATE_ACTUAL:
		return verifier_actual_str(p);
	case SCENARIO_STATE_AUDIT:
		return dynamic_str("path audit");
	case SCENARIO_STATE_ATEND:
		return dynamic_str("path at end");
	default:
		assert(false);
	}
}

static char *
verifier_abstract_str(struct verifier *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tABSTRACT\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(p->s->abstract));
	strbuilder_printf(b, "%s\n", state_str(p->s->abstract));
	return strbuilder_build(b);
}

static char *
verifier_actual_str(struct verifier *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tACTUAL\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(p->s->actual));
	strbuilder_printf(b, "%s\n", state_str(p->s->actual));
	return strbuilder_build(b);
}

static char *
verifier_setupabstract_str(struct verifier *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tSETUP (ABSTRACT)\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(p->s->abstract));
	strbuilder_printf(b, "%s\n", state_str(p->s->abstract));
	return strbuilder_build(b);
}

static char *
verifier_setupactual_str(struct verifier *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tSETUP (ACTUAL)\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(p->s->actual));
	strbuilder_printf(b, "%s\n", state_str(p->s->actual));
	return strbuilder_build(b);
}

bool
verifier_atend(struct verifier *p)
{
	if (p->issplit) {
		return mux_atend(p->mux);
	}
	return p->s->state == SCENARIO_STATE_ATEND;
}

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


/* verifier_progress */

static struct error *
verifier_init_abstract(struct verifier *p);

static struct error *
verifier_init_actual(struct verifier *p);

static struct error *
verifier_audit(struct verifier *p);

static struct error *
progress(struct verifier *, progressor *);

struct error *
verifier_progress(struct verifier *p, progressor *prog)
{
	if (p->issplit) {
		return progress(p, prog);
	}
	switch (p->s->state) {
	case SCENARIO_STATE_UNINIT:
		return verifier_init_abstract(p);
	case SCENARIO_STATE_HALFWAY:
		return verifier_init_actual(p);
	case SCENARIO_STATE_AUDIT:
		return verifier_audit(p);
	case SCENARIO_STATE_SETUPABSTRACT:
	case SCENARIO_STATE_ABSTRACT:
	case SCENARIO_STATE_SETUPACTUAL:
	case SCENARIO_STATE_ACTUAL:
		return progress(p, prog);
	case SCENARIO_STATE_ATEND:
	default:
		assert(false);
	}
}

static struct error *
verifier_init_abstract(struct verifier *p)
{
	struct error *err;

	struct frame *f = frame_callabstract_create(
		ast_function_name(p->s->f),
		ast_function_abstract(p->s->f),
		ast_expr_identifier_create(dynamic_str("base abs")), /* XXX */
		p->s->f
	);
	p->s->abstract = state_create(f, p->s->ext);
	if ((err = ast_function_initparams(p->s->f, p->s->abstract))) {
		return err;
	}
	assert(!state_readregister(p->s->abstract));
	struct frame *setupframe = frame_blockfindsetup_create(
		dynamic_str("setup"),
		ast_function_abstract(p->s->f)
	);
	state_pushframe(p->s->abstract, setupframe);
	p->s->state = SCENARIO_STATE_SETUPABSTRACT;
	return NULL;
}

static struct error *
verifier_init_actual(struct verifier *p)
{
	struct error *err;

	/* if body empty just apply setup */
	struct frame *f = frame_callactual_create(
		ast_function_name(p->s->f),
		ast_function_body(p->s->f),
		ast_expr_identifier_create(dynamic_str("base act")), /* XXX */
		p->s->f
	);
	p->s->actual = state_create(f, p->s->ext);
	if ((err = ast_function_initparams(p->s->f, p->s->actual))) {
		return err;
	}
	state_setrconsts(p->s->actual, p->s->abstract);
	struct frame *setupframe = frame_blockfindsetup_create(
		dynamic_str("setup"),
		ast_function_abstract(p->s->f)
	);
	state_pushframe(p->s->actual, setupframe);
	p->s->state = SCENARIO_STATE_SETUPACTUAL;
	return NULL;
}

static struct error *
verifier_audit(struct verifier *p)
{
	if (state_hasgarbage(p->s->actual)) {
		v_printf("actual: %s", state_str(p->s->actual));
		return error_printf(
			"%s: garbage on heap", ast_function_name(p->s->f)
		);
	}
	if (!state_equal(p->s->actual, p->s->abstract)) {
		/* unequal states are printed by state_equal so that the user
		 * can see the states with undeclared vars */
		return error_printf(
			"%s: actual and abstract states differ",
			ast_function_name(p->s->f)
		);
	}
	p->s->state = SCENARIO_STATE_ATEND;
	return NULL;
}

static struct error *
progressact(struct verifier *, progressor *);

static void
verifierinstruct_do(struct verifierinstruct *, struct verifier *);

static struct error *
progress(struct verifier *p, progressor *prog)
{
	struct error *err = progressact(p, prog);
	if (err) {
		struct error *inst_err = error_to_verifierinstruct(err);
		if (!inst_err) {
			return err;
		}
		verifierinstruct_do(error_get_verifierinstruct(inst_err), p);
	}
	return NULL;
}

static struct error *
verifier_progress_setupabstract(struct verifier *p, progressor *);

static struct error *
verifier_progress_abstract(struct verifier *p, progressor *);

static struct error *
verifier_progress_setupactual(struct verifier *p, progressor *);

static struct error *
verifier_progress_actual(struct verifier *p, progressor *);

static struct error *
verifier_progress_split(struct verifier *p, progressor *);

static struct error *
progressact(struct verifier *p, progressor *prog)
{
	if (p->issplit) {
		return verifier_progress_split(p, prog);
	}
	switch (p->s->state) {
	case SCENARIO_STATE_SETUPABSTRACT:
		return verifier_progress_setupabstract(p, prog);
	case SCENARIO_STATE_ABSTRACT:
		return verifier_progress_abstract(p, prog);
	case SCENARIO_STATE_SETUPACTUAL:
		return verifier_progress_setupactual(p, prog);
	case SCENARIO_STATE_ACTUAL:
		return verifier_progress_actual(p, prog);
	default:
		assert(false);
	}
}

static struct error *
progressortrace(struct state *, progressor *);

static struct error *
verifier_progress_setupabstract(struct verifier *p, progressor *prog)
{
	if (state_atsetupend(p->s->abstract)) {
		p->s->state = SCENARIO_STATE_ABSTRACT;
		return NULL;
	}
	return progressortrace(p->s->abstract, prog);
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
verifier_progress_abstract(struct verifier *p, progressor *prog)
{	
	if (state_atend(p->s->abstract)) {
		p->s->state = SCENARIO_STATE_HALFWAY;
		return NULL;
	}
	return progressortrace(p->s->abstract, prog);
}

static struct error *
verifier_progress_setupactual(struct verifier *p, progressor *prog)
{
	if (state_atsetupend(p->s->actual)) {
		p->s->state = SCENARIO_STATE_ACTUAL;
		return NULL;
	}
	return progressortrace(p->s->actual, prog);
}

static struct error *
verifier_progress_actual(struct verifier *p, progressor *prog)
{	
	if (state_atend(p->s->actual)) {
		p->s->state = SCENARIO_STATE_AUDIT;
		return NULL;
	}
	return progressortrace(p->s->actual, prog);
}

static struct error *
verifier_progress_split(struct verifier *p, progressor *prog)
{
	struct verifier *branch = mux_activeverifier(p->mux);
	if (verifier_atend(branch)) {
		mux_next(p->mux);
		return NULL;
	}
	return verifier_progress(branch, prog);
}


/* verifier_split */

static struct verifier_arr *
verifier_gensplits(struct verifier *, struct splitinstruct *);

static void
verifier_split(struct verifier *p, struct splitinstruct *inst)
{
	p->mux = mux_create(verifier_gensplits(p, inst));
	/* TODO: destroy abstract and actual */
	p->s->abstract = NULL;
	p->s->actual = NULL;
	p->issplit = true;
}

static struct verifier *
verifier_copywithsplit(struct verifier *old, struct map *split);

static struct verifier_arr *
verifier_gensplits(struct verifier *p, struct splitinstruct *inst)
{
	struct verifier_arr *arr = verifier_arr_create();;
	struct map **split = splitinstruct_splits(inst);
	int n = splitinstruct_n(inst);
	for (int i = 0; i < n; i++) {
		verifier_arr_append(arr, verifier_copywithsplit(p, split[i]));
	}
	return arr;
}


static struct ast_function *
copy_withsplitname(struct ast_function *, struct map *split); 

static struct verifier *
verifier_copywithsplit(struct verifier *old, struct map *split)
{
	struct verifier *p = verifier_create(
		copy_withsplitname(old->s->f, split), old->s->ext
	);
	char *fname = ast_function_name(p->s->f);
	p->s->state = old->s->state;
	switch (old->s->state) {
	case SCENARIO_STATE_SETUPABSTRACT:
	case SCENARIO_STATE_ABSTRACT:
		p->s->abstract = state_copywithname(old->s->abstract, fname);
		if (!state_split(p->s->abstract, split)) {
			p->s->state = SCENARIO_STATE_ATEND;
		}
		break;
	case SCENARIO_STATE_SETUPACTUAL:
	case SCENARIO_STATE_ACTUAL:
		p->s->abstract = state_copywithname(old->s->abstract, fname);
		p->s->actual = state_copywithname(old->s->actual, fname);
		if (!state_split(p->s->actual, split)) {
			p->s->state = SCENARIO_STATE_ATEND;
		}
		state_setrconsts(p->s->abstract, p->s->actual);
		break;
	default:
		assert(false);
	}
	return p;
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


struct error *
verifier_verify(struct verifier *p, struct ast_expr *expr)
{
	if (p->issplit) {
		return verifier_verify(mux_activeverifier(p->mux), expr);
	}
	switch(p->s->state) {
	case SCENARIO_STATE_ABSTRACT:
		return ast_stmt_verify(ast_stmt_create_expr(NULL, expr), p->s->abstract);
	case SCENARIO_STATE_ACTUAL:
		return ast_stmt_verify(ast_stmt_create_expr(NULL, expr), p->s->actual);	
	case SCENARIO_STATE_UNINIT:
	case SCENARIO_STATE_HALFWAY:
	case SCENARIO_STATE_AUDIT:
	case SCENARIO_STATE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}

struct lexememarker *
verifier_lexememarker(struct verifier *p)
{
	if (p->issplit) {
		return verifier_lexememarker(mux_activeverifier(p->mux));
	}
	switch (p->s->state) {
	case SCENARIO_STATE_SETUPABSTRACT:
	case SCENARIO_STATE_ABSTRACT:
		return state_lexememarker(p->s->abstract);
	case SCENARIO_STATE_SETUPACTUAL:
	case SCENARIO_STATE_ACTUAL:
		return state_lexememarker(p->s->actual);	
	case SCENARIO_STATE_UNINIT:
	case SCENARIO_STATE_HALFWAY:
	case SCENARIO_STATE_AUDIT:
	case SCENARIO_STATE_ATEND:
		return NULL;
	default:
		assert(false);
	}
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
	return mux->index == verifier_arr_n(mux->verifiers)-1
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
