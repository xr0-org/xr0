#ifndef XR0_AST_RESULT_H
#define XR0_AST_RESULT_H
#include <stdbool.h>

struct value;
struct error;

struct result;

struct result *
result_error_create(struct error *err);

struct result *
result_value_create(struct value *val);

void
result_destroy(struct result *);

bool
result_iserror(struct result *);

struct error *
result_as_error(struct result *);

struct value *
result_as_value(struct result *);

bool
result_hasvalue(struct result *);

struct object;
struct ast_type;

struct lvalue *
lvalue_create(struct ast_type *, struct object *);

void
lvalue_destroy(struct lvalue *);

struct ast_type *
lvalue_type(struct lvalue *);

struct object *
lvalue_object(struct lvalue *);

struct preresult;

struct preresult *
preresult_empty_create();

struct preresult *
preresult_error_create(struct error *err);

struct preresult *
preresult_contradiction_create();

void
preresult_destroy(struct preresult *);

bool
preresult_isempty(struct preresult *);

bool
preresult_iserror(struct preresult *);

struct error *
preresult_as_error(struct preresult *);

bool
preresult_iscontradiction(struct preresult *);

#endif
