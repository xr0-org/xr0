#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "ext.h"
#include "util.h"

struct ast_externdecl {
	enum ast_externdecl_kind {
		EXTERN_FUNCTION,
		EXTERN_VARIABLE,
		EXTERN_TYPEDEF,
		EXTERN_STRUCT,
		EXTERN_INCLUDE,
	} kind;
	union {
		struct ast_function *function;
		struct ast_variable *variable;
		struct {
			char *name;
			struct ast_type *type;
		} _typedef;
		struct ast_type *_struct;
		char *include;
	};
};

struct ast_externdecl *
ast_functiondecl_create(struct ast_function *f)
{
	struct ast_externdecl *decl = malloc(sizeof(struct ast_externdecl));
	decl->kind = EXTERN_FUNCTION;
	decl->function = f;
	return decl;
}

bool
ast_externdecl_isfunction(struct ast_externdecl *decl)
{
	return decl->kind == EXTERN_FUNCTION;
}

struct ast_function *
ast_externdecl_as_function(struct ast_externdecl *decl)
{
	assert(decl->kind == EXTERN_FUNCTION);
	return decl->function;
}

struct ast_externdecl *
ast_decl_create(char *name, struct ast_type *t)
{
	struct ast_externdecl *decl = malloc(sizeof(struct ast_externdecl));
	if (ast_type_istypedef(t)) {
		decl->kind = EXTERN_TYPEDEF;
		decl->_typedef.name = name;
		decl->_typedef.type = t;
	} else if (ast_type_isstruct(t)) {
		assert(ast_type_struct_tag(t));
		decl->kind = EXTERN_STRUCT;
		decl->_struct = t;
	} else { /* variable */
		decl->kind = EXTERN_VARIABLE;
		decl->variable = ast_variable_create(name, t);
	}
	return decl;
}

struct ast_externdecl *
ast_include_create(char *filename)
{
	struct ast_externdecl *decl = malloc(sizeof(struct ast_externdecl));
	decl->kind = EXTERN_INCLUDE;
	decl->include = filename;
	return decl;
}

void
ast_externdecl_install(struct ast_externdecl *decl, struct externals *ext)
{
	struct ast_function *f;
	struct ast_variable *v;

	switch (decl->kind) {
	case EXTERN_FUNCTION:
		f = decl->function;
		externals_declarefunc(ext, ast_function_name(f), f);
		break;
	case EXTERN_VARIABLE:
		v = decl->variable;
		externals_declarevar(ext, ast_variable_name(v), v);
		break;
	case EXTERN_TYPEDEF:
		externals_declaretypedef(
			ext, decl->_typedef.name, decl->_typedef.type
		);
		break;
	case EXTERN_STRUCT:
		externals_declarestruct(ext, decl->_struct);
		break;
	case EXTERN_INCLUDE:
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
		ast_function_destroy(decl->function);
		break;
	case EXTERN_VARIABLE:
		ast_variable_destroy(decl->variable);
		break;
	case EXTERN_TYPEDEF:
		free(decl->_typedef.name);
		ast_type_destroy(decl->_typedef.type);
		break;
	case EXTERN_STRUCT:
		ast_type_destroy(decl->_struct);
		break;
	case EXTERN_INCLUDE:
		free(decl->include);
		break;
	default:
		assert(false);
	}
	free(decl);
}

static char *
typedef_initprint(struct ast_externdecl *, struct externals *);

static char *
struct_initprint(struct ast_externdecl *, struct externals *);

static char *
include_initprint(char *);

char *
ast_externdecl_initprint(struct ast_externdecl *decl, struct externals *ext)
{
	switch (decl->kind) {
	case EXTERN_FUNCTION:
		return ast_function_initprint(decl->function, ext);
	case EXTERN_VARIABLE:
		/* TODO: globals */
		assert(false);
	case EXTERN_TYPEDEF:
		return typedef_initprint(decl, ext);
	case EXTERN_STRUCT:
		return struct_initprint(decl, ext);
	case EXTERN_INCLUDE:
		return include_initprint(decl->include);
	default:
		assert(false);
	}
}

static char *
typedef_initprint(struct ast_externdecl *decl, struct externals *ext)
{
	assert(decl->kind == EXTERN_TYPEDEF);
	struct strbuilder *b = strbuilder_create();
	char *t = ast_type_str(decl->_typedef.type);
	strbuilder_printf(b, "%s %s;\n", t, decl->_typedef.name);
	free(t);
	return strbuilder_build(b);
}

static char *
struct_initprint(struct ast_externdecl *decl, struct externals *ext)
{
	struct strbuilder *b = strbuilder_create();
	char *s = ast_type_str(decl->_struct);
	strbuilder_printf(b, "%s;\n", s);
	free(s);
	return strbuilder_build(b);
}

static char *
include_initprint(char *s)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "#include %s\n", s);
	return strbuilder_build(b);
}
