#include <stdlib.h>
#include <assert.h>
#include "ast.h"
#include "util.h"

struct externals {
	struct map *func, *var, *_typedef, *_struct;
};

struct externals *
externals_create()
{
	struct externals *ext = malloc(sizeof(struct externals));
	ext->func = map_create();
	ext->var = map_create();
	ext->_typedef = map_create();
	ext->_struct = map_create();
	return ext;
}

void
externals_destroy(struct externals *ext)
{
	map_destroy(ext->func);
	map_destroy(ext->var);
	map_destroy(ext->_typedef);
	map_destroy(ext->_struct);
	free(ext);
}

char *
externals_types_str(struct externals *ext, char *indent)
{
	struct strbuilder *b = strbuilder_create();

	struct map *m = ext->_typedef;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		char *type = ast_type_str((struct ast_type *) e.value);
		strbuilder_printf(b, "%s%s %s\n", indent, type, e.key);
		free(type);
	}
	m = ext->_struct;
	for (int i = 0; i < m->n; i++) {
		char *type = ast_type_str((struct ast_type *) m->entry[i].value);
		strbuilder_printf(b, "%s%s\n", indent, type);
		free(type);
	}


	return strbuilder_build(b);
}

void
externals_declarefunc(struct externals *ext, char *id, struct ast_function *f)
{
	map_set(ext->func, dynamic_str(id), f);
}

void
externals_declarevar(struct externals *ext, char *id, struct ast_variable *v)
{
	map_set(ext->var, dynamic_str(id), v);
}

void
externals_declaretypedef(struct externals *ext, char *id, struct ast_type *t)
{
	map_set(ext->_typedef, dynamic_str(id), t);
}

void
externals_declarestruct(struct externals *ext, struct ast_type *t)
{
	char *id = ast_type_struct_tag(t);
	assert(id);
	map_set(ext->_typedef, dynamic_str(id), t);
}



struct ast_function *
externals_getfunc(struct externals *ext, char *id)
{
	return map_get(ext->func, id);
}

struct ast_type *
externals_gettypedef(struct externals *ext, char *id)
{
	return map_get(ext->_typedef, id);
}

struct ast_type *
externals_getstruct(struct externals *ext, char *id)
{
	return map_get(ext->_struct, id);
}
