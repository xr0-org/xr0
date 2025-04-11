#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "util.h"

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
jump_isreturn(struct jump *j)
{
	return j->t == RETURN;
}

int
jump_hasrv(struct jump *j)
{
	return jump_isreturn(j) && j->u.rv;
}

struct ast_expr *
jump_rv(struct jump *j)
{
	assert(jump_isreturn(j));
	assert(jump_hasrv(j));
	return j->u.rv;
}

int
jump_isbreak(struct jump *j)
{
	return j->t == BREAK;
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
