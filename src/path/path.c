#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
#include "object.h"
#include "state.h"
#include "path.h"
#include "value.h"

struct path {
	enum path_state {
		PATH_STATE_UNINIT,
		PATH_STATE_SETUPABSTRACT,
		PATH_STATE_ABSTRACT,
		PATH_STATE_HALFWAY,
		PATH_STATE_SETUPACTUAL,
		PATH_STATE_ACTUAL,
		PATH_STATE_AUDIT,
		PATH_STATE_SPLIT,
		PATH_STATE_ATEND,
	} path_state;
	struct state *abstract, *actual;
	
	int branch_index;
	struct path_arr *paths;

	struct ast_function *f;
	struct externals *ext;
};

struct path_arr {
	int n;
	struct path **paths;
};

static struct path_arr *
path_arr_create()
{
	struct path_arr *arr = calloc(1, sizeof(struct path_arr));
	assert(arr);
	return arr;
}

static void
path_arr_destroy(struct path_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		path_destroy(arr->paths[i]);
	}
	free(arr->paths);
	free(arr);
}

static int
path_arr_append(struct path_arr *arr, struct path *p)
{
	arr->paths = realloc(arr->paths, sizeof(struct path_arr) * ++arr->n);
	assert(arr->paths);
	int loc = arr->n-1;
	arr->paths[loc] = p;
	return loc;
}

static bool
path_arr_atend(struct path_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		if (!path_atend(arr->paths[i])) {
			return false;	
		}
	}
	return true;
}

struct path *
path_create(struct ast_function *f, struct externals *ext)
{
	struct path *p = calloc(1, sizeof(struct path));
	p->f = ast_function_copy(f);
	p->ext = ext;
	p->paths = path_arr_create();
	p->path_state = PATH_STATE_UNINIT;
	return p;
}

void
path_destroy(struct path *p)
{
	assert(path_atend(p));

	/*state_destroy(p->abstract);*/
	/*state_destroy(p->actual);*/
	path_arr_destroy(p->paths);
	ast_function_destroy(p->f);
	free(p);
}

char *
path_setupabstract_str(struct path *);

char *
path_abstract_str(struct path *);

char *
path_setupactual_str(struct path *);

char *
path_actual_str(struct path *);

char *
path_split_str(struct path *);

char *
path_str(struct path *p)
{
	switch (p->path_state) {
	case PATH_STATE_UNINIT:
		return dynamic_str("path init abstract state");
	case PATH_STATE_SETUPABSTRACT:
		return path_setupabstract_str(p);
	case PATH_STATE_ABSTRACT:
		return path_abstract_str(p);
	case PATH_STATE_HALFWAY:
		return dynamic_str("path init actual state");
	case PATH_STATE_SETUPACTUAL:
		return path_setupactual_str(p);
	case PATH_STATE_ACTUAL:
		return path_actual_str(p);
	case PATH_STATE_AUDIT:
		return dynamic_str("path audit");
	case PATH_STATE_SPLIT:
		return path_split_str(p);
	case PATH_STATE_ATEND:
		return dynamic_str("path at end");
	default:
		assert(false);
	}
}

char *
path_abstract_str(struct path *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tABSTRACT\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(p->abstract));
	strbuilder_printf(b, "%s\n", state_str(p->abstract));
	return strbuilder_build(b);
}

char *
path_actual_str(struct path *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tACTUAL\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(p->actual));
	strbuilder_printf(b, "%s\n", state_str(p->actual));
	return strbuilder_build(b);
}

char *
path_setupabstract_str(struct path *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tSETUP (ABSTRACT)\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(p->abstract));
	strbuilder_printf(b, "%s\n", state_str(p->abstract));
	return strbuilder_build(b);
}

char *
path_setupactual_str(struct path *p)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "phase:\tSETUP (ACTUAL)\n\n");
	strbuilder_printf(b, "text:\n%s\n", state_programtext(p->actual));
	strbuilder_printf(b, "%s\n", state_str(p->actual));
	return strbuilder_build(b);
}

char *
path_split_str(struct path *p)
{
	struct path *branch = p->paths->paths[p->branch_index];
	return path_str(branch);
}

static void
path_nextbranch(struct path *p)
{
	assert(p->paths->n >= 2);
	int index = p->branch_index;
	if (index < p->paths->n - 1) {
		p->branch_index++;	
	}
}

bool
path_atend(struct path *p)
{
	switch (p->path_state) {
	case PATH_STATE_SPLIT:
		return path_arr_atend(p->paths);
	case PATH_STATE_ATEND:
		return true;
	default:
		return false;
	}
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


/* path_progress */

static struct error *
path_init_abstract(struct path *p);

static struct error *
path_init_actual(struct path *p);

static struct error *
path_audit(struct path *p);

static struct error *
progress(struct path *, progressor *);

struct error *
path_progress(struct path *p, progressor *prog)
{
	switch (p->path_state) {
	case PATH_STATE_UNINIT:
		return path_init_abstract(p);
	case PATH_STATE_HALFWAY:
		return path_init_actual(p);
	case PATH_STATE_AUDIT:
		return path_audit(p);
	case PATH_STATE_SETUPABSTRACT:
	case PATH_STATE_ABSTRACT:
	case PATH_STATE_SETUPACTUAL:
	case PATH_STATE_ACTUAL:
	case PATH_STATE_SPLIT:
		return progress(p, prog);
	case PATH_STATE_ATEND:
	default:
		assert(false);
	}
}

static struct error *
path_init_abstract(struct path *p)
{
	struct error *err;

	struct frame *f = frame_callabstract_create(
		ast_function_name(p->f),
		ast_function_abstract(p->f),
		ast_expr_identifier_create(dynamic_str("base abs")), /* XXX */
		p->f
	);
	p->abstract = state_create(f, p->ext);
	if ((err = ast_function_initparams(p->f, p->abstract))) {
		return err;
	}
	assert(!state_readregister(p->abstract));
	struct frame *setupframe = frame_blockfindsetup_create(
		dynamic_str("setup"),
		ast_function_abstract(p->f)
	);
	state_pushframe(p->abstract, setupframe);
	p->path_state = PATH_STATE_SETUPABSTRACT;
	return NULL;
}

static struct error *
path_init_actual(struct path *p)
{
	struct error *err;

	/* if body empty just apply setup */
	struct frame *f = frame_callactual_create(
		ast_function_name(p->f),
		ast_function_body(p->f),
		ast_expr_identifier_create(dynamic_str("base act")), /* XXX */
		p->f
	);
	p->actual = state_create(f, p->ext);
	if ((err = ast_function_initparams(p->f, p->actual))) {
		return err;
	}
	state_setrconsts(p->actual, p->abstract);
	struct frame *setupframe = frame_blockfindsetup_create(
		dynamic_str("setup"),
		ast_function_abstract(p->f)
	);
	state_pushframe(p->actual, setupframe);
	p->path_state = PATH_STATE_SETUPACTUAL;
	return NULL;
}

static struct error *
path_audit(struct path *p)
{
	if (state_hasgarbage(p->actual)) {
		v_printf("actual: %s", state_str(p->actual));
		return error_printf(
			"%s: garbage on heap", ast_function_name(p->f)
		);
	}
	if (!state_equal(p->actual, p->abstract)) {
		/* unequal states are printed by state_equal so that the user
		 * can see the states with undeclared vars */
		return error_printf(
			"%s: actual and abstract states differ",
			ast_function_name(p->f)
		);
	}
	p->path_state = PATH_STATE_ATEND;
	return NULL;
}

static struct error *
progressact(struct path *, progressor *);

static void
pathinstruct_do(struct pathinstruct *, struct path *);

static struct error *
progress(struct path *p, progressor *prog)
{
	struct error *err = progressact(p, prog);
	if (err) {
		struct error *inst_err = error_to_pathinstruct(err);
		if (!inst_err) {
			return err;
		}
		pathinstruct_do(error_get_pathinstruct(inst_err), p);
	}
	return NULL;
}

static struct error *
path_progress_setupabstract(struct path *p, progressor *);

static struct error *
path_progress_abstract(struct path *p, progressor *);

static struct error *
path_progress_setupactual(struct path *p, progressor *);

static struct error *
path_progress_actual(struct path *p, progressor *);

static struct error *
path_progress_split(struct path *p, progressor *);

static struct error *
progressact(struct path *p, progressor *prog)
{
	switch (p->path_state) {
	case PATH_STATE_SETUPABSTRACT:
		return path_progress_setupabstract(p, prog);
	case PATH_STATE_ABSTRACT:
		return path_progress_abstract(p, prog);
	case PATH_STATE_SETUPACTUAL:
		return path_progress_setupactual(p, prog);
	case PATH_STATE_ACTUAL:
		return path_progress_actual(p, prog);
	case PATH_STATE_SPLIT:
		return path_progress_split(p, prog);
	default:
		assert(false);
	}
}

static struct error *
progressortrace(struct state *, progressor *);

static struct error *
path_progress_setupabstract(struct path *p, progressor *prog)
{
	if (state_atsetupend(p->abstract)) {
		p->path_state = PATH_STATE_ABSTRACT;
		return NULL;
	}
	return progressortrace(p->abstract, prog);
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
path_progress_abstract(struct path *p, progressor *prog)
{	
	if (state_atend(p->abstract)) {
		p->path_state = PATH_STATE_HALFWAY;
		return NULL;
	}
	return progressortrace(p->abstract, prog);
}

static struct error *
path_progress_setupactual(struct path *p, progressor *prog)
{
	if (state_atsetupend(p->actual)) {
		p->path_state = PATH_STATE_ACTUAL;
		return NULL;
	}
	return progressortrace(p->actual, prog);
}

static struct error *
path_progress_actual(struct path *p, progressor *prog)
{	
	if (state_atend(p->actual)) {
		p->path_state = PATH_STATE_AUDIT;
		return NULL;
	}
	return progressortrace(p->actual, prog);
}

static struct error *
branch_progress(struct path *, struct path *, progressor *);

static struct error *
path_progress_split(struct path *p, progressor *prog)
{
	/* path_atend holds this invariant whenever this function is called */ 
	assert(!path_arr_atend(p->paths));
	return branch_progress(
		p, p->paths->paths[p->branch_index], prog
	);
}

static struct error *
branch_progress(struct path *parent, struct path *branch, progressor *prog)
{
	if (path_atend(branch)) {
		path_nextbranch(parent);
		return NULL;
	}
	return path_progress(branch, prog);
}


/* path_split */

static struct path *
path_copywithsplit(struct path *old, struct map *split);

static void
path_split(struct path *p, struct splitinstruct *inst)
{
	struct map **split = splitinstruct_splits(inst);
	int n = splitinstruct_n(inst);
	for (int i = 0; i < n; i++) {
		path_arr_append(p->paths, path_copywithsplit(p, split[i]));
	}
	/* TODO: destroy abstract and actual */
	p->abstract = NULL;
	p->actual = NULL;
	p->path_state = PATH_STATE_SPLIT;
}

static struct ast_function *
copy_withsplitname(struct ast_function *, struct map *split); 

static struct path *
path_copywithsplit(struct path *old, struct map *split)
{
	struct path *p = path_create(
		copy_withsplitname(old->f, split), old->ext
	);
	char *fname = ast_function_name(p->f);
	p->path_state = old->path_state;
	switch (old->path_state) {
	case PATH_STATE_SETUPABSTRACT:
	case PATH_STATE_ABSTRACT:
		p->abstract = state_copywithname(old->abstract, fname);
		if (!state_split(p->abstract, split)) {
			p->path_state = PATH_STATE_ATEND;
		}
		break;
	case PATH_STATE_SETUPACTUAL:
	case PATH_STATE_ACTUAL:
		p->abstract = state_copywithname(old->abstract, fname);
		p->actual = state_copywithname(old->actual, fname);
		if (!state_split(p->actual, split)) {
			p->path_state = PATH_STATE_ATEND;
		}
		state_setrconsts(p->abstract, p->actual);
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


static struct error *
path_split_verify(struct path *, struct ast_expr *);

struct error *
path_verify(struct path *p, struct ast_expr *expr)
{
	switch(p->path_state) {
	case PATH_STATE_ABSTRACT:
		return ast_stmt_verify(ast_stmt_create_expr(NULL, expr), p->abstract);
	case PATH_STATE_ACTUAL:
		return ast_stmt_verify(ast_stmt_create_expr(NULL, expr), p->actual);	
	case PATH_STATE_SPLIT:
		return path_split_verify(p, expr);
	case PATH_STATE_UNINIT:
	case PATH_STATE_HALFWAY:
	case PATH_STATE_AUDIT:
	case PATH_STATE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}

static struct error *
path_split_verify(struct path *p, struct ast_expr *expr)
{
	struct path *branch = p->paths->paths[p->branch_index];
	return path_verify(branch, expr);
}

static struct lexememarker *
path_split_lexememarker(struct path *);

struct lexememarker *
path_lexememarker(struct path *p)
{
	switch (p->path_state) {
	case PATH_STATE_SETUPABSTRACT:
	case PATH_STATE_ABSTRACT:
		return state_lexememarker(p->abstract);
	case PATH_STATE_SETUPACTUAL:
	case PATH_STATE_ACTUAL:
		return state_lexememarker(p->actual);	
	case PATH_STATE_SPLIT:
		return path_split_lexememarker(p);	
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
path_split_lexememarker(struct path *p)
{
	struct path *branch = p->paths->paths[p->branch_index];
	return path_lexememarker(branch);
}


struct pathinstruct {
	enum pathinstruct_type {
		PATHINSTRUCT_SPLIT,
	} type;
	union {
		struct splitinstruct *split;
	};
};

struct pathinstruct *
pathinstruct_split(struct splitinstruct *s)
{
	struct pathinstruct *inst = malloc(sizeof(struct pathinstruct));
	assert(inst);
	inst->type = PATHINSTRUCT_SPLIT;
	inst->split = s;
	return inst;
}

static void
pathinstruct_do(struct pathinstruct *inst, struct path *p)
{
	switch (inst->type) {
	case PATHINSTRUCT_SPLIT:
		path_split(p, inst->split);
		break;
	default:
		assert(false);
	}
}
