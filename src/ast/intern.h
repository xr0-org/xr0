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

struct iresult;

struct iresult *
iresult_error_create(struct error *err);

struct iresult *
iresult_expr_create(struct ast_expr *val);

void
iresult_destroy(struct iresult *);

bool
iresult_iserror(struct iresult *);

struct error *
iresult_as_error(struct iresult *);

struct ast_expr *
iresult_as_expr(struct iresult *);

bool
iresult_hasexpr(struct iresult *);

#endif
