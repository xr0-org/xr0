#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "util.h"
#include "state.h"
#include "value.h"

#include "type.h"
#include "expr.h"

#include "jump.h"

struct jump {
	enum type {
		BREAK,
		RETURN,
		GOTO,
	} t;
	union {
		struct ast_expr *rv;
		char *label;
	} u;
};

static struct jump *
jump_create(enum type t)
{
	struct jump *j = malloc(sizeof(struct jump));
	assert(j);
	j->t = t;
	return j;
}

struct jump *
jump_break_create(void)
{
	return jump_create(BREAK);
}

struct jump *
jump_return_create(struct ast_expr *rv)
{
	struct jump *j = jump_create(RETURN);
	j->u.rv = rv;
	return j;
}

struct jump *
jump_goto_create(char *label)
{
	struct jump *j = jump_create(GOTO);
	j->u.label = label;
	return j;
}

struct jump *
jump_copy(struct jump *old)
{
	struct jump *new = jump_create(old->t);
	switch (old->t) {
	case RETURN:
		if (old->u.rv)
			new->u.rv = ast_expr_copy(old->u.rv);
		break;
	case GOTO:
		new->u.label = dynamic_str(old->u.label);
		break;
	default:
		assert(0);
	}
	return new;
}

void
jump_destroy(struct jump *j)
{
	switch (j->t) {
	case RETURN:
		if (j->u.rv)
			ast_expr_destroy(j->u.rv);
		break;
	case GOTO:
		free(j->u.label);
		break;
	default:
		assert(0);
	}
	free(j);
}

static char *
return_str(struct jump *);

static char *
goto_str(struct jump *);

char *
jump_str(struct jump *j)
{
	switch (j->t) {
	case BREAK:
		return dynamic_str("break;");
	case RETURN:
		return return_str(j);
	case GOTO:
		return goto_str(j);
	default:
		assert(false);
	}
}

static char *
return_str(struct jump *j)
{
	struct strbuilder *b = strbuilder_create();
	if (j->u.rv) {
		char *rv = ast_expr_str(j->u.rv);
		strbuilder_printf(b, "return %s;", rv);
		free(rv);
	} else {
		strbuilder_printf(b, "return;");
	}
	return strbuilder_build(b);
}

static char *
goto_str(struct jump *j)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "goto %s;", j->u.label);
	return strbuilder_build(b);
}

int
jump_isbreak(struct jump *j)
{
	return j->t == BREAK;
}

int
jump_isreturn(struct jump *j)
{
	return j->t == RETURN;
}

struct ast_expr *
jump_rv(struct jump *j)
{
	assert(jump_isreturn(j) && j->u.rv);
	return j->u.rv;
}

int
jump_islinearisable(struct jump *j)
{
	return j->t == RETURN && j->u.rv;
}

struct string_arr *
jump_getfuncs(struct jump *j)
{
	switch (j->t) {
	case RETURN:
		if (j->u.rv)
			return ast_expr_getfuncs(j->u.rv);
	case GOTO:
		break;
	default:
		assert(0);
	}
	return string_arr_create();
}

static struct error *
return_exec(struct jump *, struct state *);

struct error *
jump_exec(struct jump *j, struct state *s)
{
	switch (j->t) {
	case RETURN:
		return return_exec(j, s);
	case GOTO:
		return error_goto(j->u.label);
	default:
		assert(0);
	}
}

static int
compatible(struct eval *ret, struct ast_type *spec_t, struct state *);

static struct error *
return_exec(struct jump *j, struct state *s)
{
	if (j->u.rv) {
		struct ast_expr *rv = jump_rv(j);
		struct e_res *res = ast_expr_eval(rv, s);
		if (e_res_iserror(res)) {
			struct error *err = e_res_as_error(res);
			if (error_to_eval_void(err)) {
				e_res_errorignore(res);
			} else {
				return err;
			}
		}
		if (e_res_haseval(res)) {
			struct eval *eval = e_res_as_eval(res);
			struct ast_type *spec_t = state_getreturntype(s);
			if (!compatible(eval, spec_t, s)) {
				char *spec_t_str = ast_type_str(spec_t),
				     *rv_t_str = ast_type_str(eval_type(eval));
				struct error *err = error_printf(
					"cannot return %s as %s",
					rv_t_str,
					spec_t_str
				);
				free(rv_t_str);
				free(spec_t_str);
				return err;
			}
			struct value_res *v_res = eval_to_value(eval, s);
			if (!value_res_hasvalue(v_res)) {
				return error_printf(
					"returned expression has no value"
				);
			}
			struct value *v = value_copy(value_res_as_value(v_res));
			state_writeregister(s, v);
		}
	}
	return error_return();
}

static int
compatible(struct eval *e, struct ast_type *spec_t, struct state *s)
{
	return ast_type_compatible(eval_type(e), spec_t) || (
		ast_type_compatiblewithrconst(spec_t)
		&& value_isrconst(value_res_as_value(eval_to_value(e, s)))
	);
}
