#ifndef XR0_AST_RESULT_H
#define XR0_AST_RESULT_H
#include <stdbool.h>

struct error;

struct ast_type;
struct location;
struct value;

struct state;

struct value_res;

struct value_res *
eval_to_value(struct eval *, struct state *);

struct object_res;

struct object_res *
eval_to_object(struct eval *, struct state *, bool constructive);

struct preresult;

struct preresult *
preresult_empty_create(void);

struct preresult *
preresult_error_create(struct error *err);

struct preresult *
preresult_contradiction_create(void);

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

struct namedseq;

struct namedseq *
namedseq_create(char *name);

char *
namedseq_next(struct namedseq *);

void
namedseq_destroy(struct namedseq *);

#endif
