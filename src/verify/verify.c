#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ext.h"
#include "ast.h"
#include "object.h"
#include "state.h"
#include "util.h"
#include "value.h"
#include "verify.h"

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state,
		struct externals *);

static struct error *
parameterise_state(struct state *s, struct ast_function *f);

struct error *
path_verify(struct ast_function *f, struct state *state, struct externals *ext)
{
	struct error *err = NULL;

	struct ast_block *body = ast_function_body(f);

	if ((err = parameterise_state(state, f))) {
		return err;
	}

	int ndecls = ast_block_ndecls(body);
	struct ast_variable **var = ast_block_decls(body);
	for (int i = 0; i < ndecls; i++) {
		state_declare(state, var[i], false);
	}

	int nstmts = ast_block_nstmts(body);
	struct ast_stmt **stmt = ast_block_stmts(body);
	for (int i = 0; i < nstmts; i++) {
		struct ast_stmt *s = stmt[i];
		/* TODO: deal with pathing logic so that we only have one path
		 * to terminal points for the function */
		enum ast_stmt_kind kind = ast_stmt_kind(s);
		if (kind == STMT_COMPOUND_V) {
			if ((err = ast_stmt_verify(s, state))) {
				return error_prepend(err, "cannot verify statement: ");
			}
		}
		if ((err = ast_stmt_exec(s, state))) {
			return error_prepend(err, "cannot exec statement: ");
		}
	}
	state_undeclarevars(state);
	/* TODO: verify that `result' is of same type as f->result */
	if ((err = abstract_audit(f, state, ext))) {
		return error_prepend(err, "qed error: ");
	}
	return NULL;
}

static bool
isprecondition(struct ast_stmt *);

static struct error *
parameterise_state(struct state *s, struct ast_function *f)
{
	/* declare params and locals in stack frame */
	struct ast_variable **param = ast_function_params(f);
	int nparams = ast_function_nparams(f);
	for (int i = 0; i < nparams; i++) {
		struct ast_variable *p = param[i];
		state_declare(s, p, true);
		if (ast_type_base(ast_variable_type(p)) == TYPE_INT) {
			struct object *obj = state_getobject(s, ast_variable_name(p));
			assert(obj);
			object_assign(obj, state_vconst(s));
		}
	}

	struct ast_block *abs = ast_function_abstract(f);
	int nstmts = ast_block_nstmts(abs);
	struct ast_stmt **stmt = ast_block_stmts(abs);
	for (int i = 0; i < nstmts; i++) {
		struct error *err = NULL;
		if (isprecondition(stmt[i])) {
			if ((err = ast_stmt_exec(stmt[i], s))) {
				return err;
			}
		}
	}

	return NULL;
}

static bool
isprecondition(struct ast_stmt *stmt)
{
	return ast_stmt_kind(stmt) == STMT_LABELLED
		&& strcmp(ast_stmt_labelled_label(stmt), "pre") == 0;
}

static struct error *
abstract_audit(struct ast_function *f, struct state *actual_state,
		struct externals *ext)
{
	struct error *err = NULL;

	/*printf("actual: %s\n", state_str(actual_state));*/
	if (!state_hasgarbage(actual_state)) {
		return error_create("garbage on heap");
	}

	struct state *alleged_state = state_create(
		dynamic_str(ast_function_name(f)), ext, ast_function_type(f)
	);
	if ((err = parameterise_state(alleged_state, f))) {
		return err;
	}

	/* mutates alleged_state */
	struct result *res = ast_function_absexec(f, alleged_state);
	if (result_iserror(res)) {
		return result_as_error(res);
	}

	/*printf("actual: %s\n", state_str(actual_state));*/
	/*printf("alleged: %s\n", state_str(alleged_state));*/

	bool equiv = state_equal(actual_state, alleged_state);

	state_destroy(alleged_state); /* actual_state handled by caller */ 
	
	if (!equiv) {
		/* XXX: print states */
		return error_create("actual and alleged states differ");
	}

	return NULL;
}
