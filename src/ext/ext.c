#include <stdlib.h>
#include "ast.h"
#include "util.h"
#include "ast_function_map.h"
#include "ast_variable_map.h"
#include "ast_type_map.h"

struct externals {
	struct ast_function_map *func;
	struct ast_variable_map *var;
	struct ast_type_map *type;
};

struct externals *
externals_create()
{
	struct externals *ext = malloc(sizeof(struct externals));
	ext->func = ast_function_map_create();
	ext->var = ast_variable_map_create();
	ext->type = ast_type_map_create();
	return ext;
}

void
externals_destroy(struct externals *ext)
{
	ast_function_map_destroy(ext->func);
	ast_variable_map_destroy(ext->var);
	ast_type_map_destroy(ext->type);
	free(ext);
}

char *
externals_types_str(struct externals *ext, char *indent)
{
	struct strbuilder *b = strbuilder_create();

	struct ast_type_map *m = ext->type;
	for (int i = 0; i < m->n; i++) {
		char *type = ast_type_str(m->entry[i].value);
		strbuilder_printf(b, "%s%s\n", indent, type);
		free(type);
	}

	return strbuilder_build(b);
}

void
externals_declarefunc(struct externals *ext, char *id, struct ast_function *f)
{
	ast_function_map_set(ext->func, dynamic_str(id), f);
}

void
externals_declarevar(struct externals *ext, char *id, struct ast_variable *v)
{
	ast_variable_map_set(ext->var, dynamic_str(id), v);
}

void
externals_declaretype(struct externals *ext, char *id, struct ast_type *t)
{
	ast_type_map_set(ext->type, dynamic_str(id), t);
}

struct ast_function *
externals_getfunc(struct externals *ext, char *id)
{
	return ast_function_map_get(ext->func, id);
}

struct ast_type *
externals_gettype(struct externals *ext, char *id)
{
	return ast_type_map_get(ext->type, id);
}

