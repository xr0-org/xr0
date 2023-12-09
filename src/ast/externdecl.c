#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "ext.h"
#include "util.h"

struct ast_externdecl {
	enum ast_externdecl_kind kind;
	union {
		struct ast_function *function;
		struct ast_variable *variable;
		struct ast_type *type;
	} u;
};

struct ast_externdecl *
ast_functiondecl_create(struct ast_function *f)
{
	struct ast_externdecl *decl = malloc(sizeof(struct ast_externdecl));
	decl->kind = EXTERN_FUNCTION;
	decl->u.function = f;
	return decl;
}

struct ast_function *
ast_externdecl_as_function(struct ast_externdecl *decl)
{
	assert(decl->kind == EXTERN_FUNCTION);
	return decl->u.function;
}

struct ast_externdecl *
ast_variabledecl_create(struct ast_variable *v)
{
	struct ast_externdecl *decl = malloc(sizeof(struct ast_externdecl));
	decl->kind = EXTERN_VARIABLE;
	decl->u.variable = v;
	return decl;
}

struct ast_externdecl *
ast_typedecl_create(struct ast_type *t)
{
	struct ast_externdecl *decl = malloc(sizeof(struct ast_externdecl));
	decl->kind = EXTERN_TYPE;
	decl->u.type = t;
	return decl;
}

enum ast_externdecl_kind
ast_externdecl_kind(struct ast_externdecl *decl)
{
	return decl->kind;
}

void
ast_externdecl_install(struct ast_externdecl *decl, struct externals *ext)
{
	struct ast_function *f;
	struct ast_variable *v;
	struct ast_type *t;

	switch (decl->kind) {
	case EXTERN_FUNCTION:
		f = decl->u.function;
		externals_declarefunc(ext, ast_function_name(f), f);
		break;
	case EXTERN_VARIABLE:
		v = decl->u.variable;
		externals_declarevar(ext, ast_variable_name(v), v);
		break;
	case EXTERN_TYPE:
		t = decl->u.type;
		assert(ast_type_base(t) == TYPE_STRUCT && ast_type_struct_tag(t));
		externals_declaretype(ext, ast_type_struct_tag(t), t);
		break;
	default:
		assert(false);
	}
}

void
ast_externdecl_destroy(struct ast_externdecl *decl)
{
	switch (decl->kind) {
	case EXTERN_FUNCTION:
		ast_function_destroy(decl->u.function);
		break;
	case EXTERN_VARIABLE:
		ast_variable_destroy(decl->u.variable);
		break;
	case EXTERN_TYPE:
		ast_type_destroy(decl->u.type);
		break;
	default:
		assert(false);
	}
	free(decl);
}
