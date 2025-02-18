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
	} t;
	struct ast_expr *rv;
};

static struct jump *
jump_create(enum type t)
{
	struct jump *j = malloc(sizeof(struct jump));
	j->t = t;
	return j;
}

struct jump *
jump_break_create()
{
	return jump_create(BREAK);
}

struct jump *
jump_return_create(struct ast_expr *rv)
{
	struct jump *j = jump_create(RETURN);
	j->rv = rv;
	return j;
}

struct jump *
jump_copy(struct jump *old)
{
	struct jump *new = jump_create(old->t);
	if (old->t == RETURN && old->rv) {
		new->rv = ast_expr_copy(old->rv);
	}
	return new;
}

void
jump_destroy(struct jump *j)
{
	if (j->t == RETURN && j->rv) {
		ast_expr_destroy(j->rv);
	}
	free(j);
}

static char *
return_str(struct jump *);

char *
jump_str(struct jump *j)
{
	switch (j->t) {
	case BREAK:
		return dynamic_str("break;");
	case RETURN:
		return return_str(j);
	default:
		assert(false);
	}
}

static char *
return_str(struct jump *j)
{
	struct strbuilder *b = strbuilder_create();
	if (j->rv) {
		char *rv = ast_expr_str(j->rv);
		strbuilder_printf(b, "return %s;", rv);
		free(rv);
	} else {
		strbuilder_printf(b, "return;");
	}
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
	return jump_isreturn(j) && j->rv;
}

struct ast_expr *
jump_rv(struct jump *j)
{
	assert(jump_isreturn(j));
	assert(jump_hasrv(j));
	return j->rv;
}

int
jump_isbreak(struct jump *j)
{
	return j->t == BREAK;
}


struct string_arr *
jump_getfuncs(struct jump *j)
{
	if (j->t == RETURN && j->rv) {
		return ast_expr_getfuncs(j->rv);
	}
	return string_arr_create();
}
