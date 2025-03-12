#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "util.h"

#include "asm.h"

struct _asm {
	enum type { SETUPV, CALL, MOV, MOVRET } t;
	union {
		struct ast_expr *call;
		char *temp_name;
		struct ast_variable *temp_var;
	} u;
	struct ast_expr *val; /* only defined for MOV */
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
asm_mov_create(char *temp, struct ast_expr *val)
{
	struct _asm *a = _create(MOV);
	a->u.temp_name = temp;
	a->val = val;
	return a;
}

struct _asm *
asm_movret_create(struct ast_variable *temp)
{
	struct _asm *a = _create(MOVRET);
	a->u.temp_var = temp;
	return a;
}

struct _asm *
asm_copy(struct _asm *old)
{
	switch (old->t) {
	case SETUPV:
		return asm_setupv_create(ast_expr_copy(old->u.call));
	case CALL:
		return asm_call_create(ast_expr_copy(old->u.call));
	case MOV:
		return asm_mov_create(
			dynamic_str(old->u.temp_name), ast_expr_copy(old->val)
		);
	case MOVRET:
		return asm_movret_create(ast_variable_copy(old->u.temp_var));
	default:
		assert(0);
	}
}

void
asm_destroy(struct _asm *a)
{
	switch (a->t) {
	case SETUPV:
	case CALL:
		ast_expr_destroy(a->u.call);
		break;
	case MOV:
		free(a->u.temp_name);
		ast_expr_destroy(a->val);
		break;
	case MOVRET:
		ast_variable_destroy(a->u.temp_var);
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

static char *
_movret_str(struct _asm *);

char *
asm_str(struct _asm *a)
{
	switch (a->t) {
	case SETUPV:
		return _call_str_with_cmd(a, "setupv");
	case CALL:
		return _call_str_with_cmd(a, "call");
	case MOV:
		return _mov_str(a);
	case MOVRET:
		return _movret_str(a);
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
	char *val = ast_expr_str(a->val);
	strbuilder_printf(b, "mov %s, %s;", a->u.temp_name, val);
	free(val);
	return strbuilder_build(b);
}

static char *
_movret_str(struct _asm *a)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "movret %s;", ast_variable_name(a->u.temp_var));
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
	return a->u.temp_var;
}
