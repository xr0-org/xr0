#ifndef XR0_AST_RESULT_H
#define XR0_AST_RESULT_H
#include <stdbool.h>

struct error;

struct ast_type;
struct location;
struct value;

struct eval;

struct eval *
eval_lval_create(struct ast_type *, struct location *);

struct eval *
eval_rval_create(struct ast_type *, struct value *);

void
eval_destroy(struct eval *);

char *
eval_str(struct eval *);

struct ast_type *
eval_type(struct eval *);

bool
eval_islval(struct eval *);

struct location *
eval_as_lval(struct eval *);

bool
eval_isrval(struct eval *);

struct value *
eval_as_rval(struct eval *);

struct state;

struct value_res;

struct value_res *
eval_to_value(struct eval *, struct state *);

struct object_res;

struct object_res *
eval_to_object(struct eval *, struct state *, bool constructive);

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
DECLARE_RESULT_TYPE(struct eval *, eval, e_res)

#endif
