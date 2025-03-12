#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "util.h"

#include "asm.h"

struct _asm {
	enum type { SETUPV, CALL, MOVRET } t;
	union {
		struct ast_expr *call;
		struct ast_variable *temp;
	} u;
};

static struct _asm *
_create(enum type t)
{
	struct _asm *a = malloc(sizeof(struct _asm));
	assert(a);
	a->t = t;
	return a;
}

struct _asm *
asm_setupv_create(struct ast_expr *call)
{
	struct _asm *a = _create(SETUPV);
	a->u.call = call;
	return a;
}

struct _asm *
asm_call_create(struct ast_expr *call)
{
	struct _asm *a = _create(CALL);
	a->u.call = call;
	return a;
}

struct _asm *
asm_movret_create(struct ast_variable *temp)
{
	struct _asm *a = _create(MOVRET);
	a->u.temp = temp;
	return a;
}

struct _asm *
asm_copy(struct _asm *old)
{
	struct _asm *new = _create(old->t);
	switch (old->t) {
	case SETUPV:
	case CALL:
		new->u.call = old->u.call;
		break;
	case MOVRET:
		new->u.temp = old->u.temp;
		break;
	default:
		assert(0);
	}
	return new;
}

void
asm_destroy(struct _asm *a)
{
	switch (a->t) {
	case SETUPV:
	case CALL:
		ast_expr_destroy(a->u.call);
		break;
	case MOVRET:
		ast_variable_destroy(a->u.temp);
		break;
	default:
		assert(0);
	}
	free(a);
}

static char *
_call_str_with_cmd(struct _asm *, char *cmd);

static char *
_mov_str(struct _asm *);

char *
asm_str(struct _asm *a)
{
	switch (a->t) {
	case SETUPV:
		return _call_str_with_cmd(a, "setupv");
	case CALL:
		return _call_str_with_cmd(a, "call");
	case MOVRET:
		return _mov_str(a);
	default:
		assert(0);
	}
}

static char *
_call_str_with_cmd(struct _asm *a, char *cmd)
{
	struct strbuilder *b = strbuilder_create();
	char *s = ast_expr_str(a->u.call);
	strbuilder_printf(b, "%s %s;", cmd, s);
	free(s);
	return strbuilder_build(b);
}

static char *
_mov_str(struct _asm *a)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "movret %s;", ast_variable_name(a->u.temp));
	return strbuilder_build(b);
}

int
asm_issetupv(struct _asm *a)
{
	return a->t == SETUPV;
}

int
asm_iscall(struct _asm *a)
{
	return a->t == CALL;
}

static int
asm_ismovret(struct _asm *a)
{
	return a->t == MOVRET;
}

struct ast_expr *
asm_getcall(struct _asm *a)
{
	assert(asm_issetupv(a) || asm_iscall(a));
	return a->u.call;
}

struct ast_variable *
asm_mov_getvar(struct _asm *a)
{
	assert(asm_ismovret(a));
	return a->u.temp;
}
