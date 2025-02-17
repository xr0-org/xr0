#ifndef XR0_AST_STMT_JUMP_H
#define XR0_AST_STMT_JUMP_H

struct jump *
jump_break_create();

struct jump *
jump_return_create(struct ast_expr *rv);

struct jump *
jump_copy(struct jump *);

void
jump_destroy(struct jump *);

char *
jump_str(struct jump *);

int
jump_isreturn(struct jump *);

int
jump_hasrv(struct jump *);

struct ast_expr;

struct ast_expr *
jump_rv(struct jump *);

int
jump_isbreak(struct jump *);

struct string_arr;

struct string_arr *
jump_getfuncs(struct jump *);

#endif
