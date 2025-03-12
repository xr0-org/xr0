#ifndef XR0_AST_STMT_ASM_H
#define XR0_AST_STMT_ASM_H

/* struct _asm: an instruction for the abstract machine Xr0 simulates. */ 
struct _asm;

struct ast_expr;

struct _asm *
asm_setupv_create(struct ast_expr *call);

struct _asm *
asm_call_create(struct ast_expr *call);

struct _asm *
asm_mov_create(char *temp, struct ast_expr *val);

struct ast_variable;

struct _asm *
asm_movret_create(struct ast_variable *temp);

struct _asm *
asm_copy(struct _asm *);

void
asm_destroy(struct _asm *);

char *
asm_str(struct _asm *);

int
asm_issetupv(struct _asm *);

int
asm_iscall(struct _asm *);

int
asm_ismov(struct _asm *);

int
asm_ismovret(struct _asm *);

struct ast_expr *
asm_getcall(struct _asm *);

char *
asm_mov_getvar(struct _asm *);

struct ast_expr *
asm_mov_getval(struct _asm *);

struct ast_variable *
asm_movret_getvar(struct _asm *);

#endif
