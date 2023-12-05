#ifndef EXT_H
#define EXT_H

struct externals;

struct externals *
externals_create();

void
externals_destroy(struct externals *);

char *
externals_types_str(struct externals *, char *indent);

struct ast_function;
struct ast_variable;
struct ast_type;

void
externals_declarefunc(struct externals *, char *id, struct ast_function *);

void
externals_declarevar(struct externals *, char *id, struct ast_variable *);

void
externals_declaretype(struct externals *, char *id, struct ast_type *type);

struct ast_function *
externals_getfunc(struct externals *, char *id);

struct ast_type *
externals_gettype(struct externals *, char *id);

#endif
