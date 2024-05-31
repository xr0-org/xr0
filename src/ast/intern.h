#ifndef XR0_AST_RESULT_H
#define XR0_AST_RESULT_H
#include <stdbool.h>

struct error;

struct ast_type;
struct object;

struct lvalue;

struct lvalue *
lvalue_create(struct ast_type *, struct object *);

void
lvalue_destroy(struct lvalue *);

struct ast_type *
lvalue_type(struct lvalue *);

struct object *
lvalue_object(struct lvalue *);

struct value;

struct rvalue;

struct rvalue *
rvalue_create(struct ast_type *, struct value *);

void
rvalue_destroy(struct rvalue *);

struct ast_type *
rvalue_type(struct rvalue *);

struct value *
rvalue_value(struct rvalue *);

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

DECLARE_RESULT_TYPE(struct ast_expr *, expr, iresult)
DECLARE_RESULT_TYPE(struct lvalue *, lvalue, l_res)
DECLARE_RESULT_TYPE(struct rvalue *, rvalue, r_res)

#endif
