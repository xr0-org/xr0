#ifndef STACK_H
#define STACK_H

#include <stdbool.h>
#include "block.h"

struct stack;

struct frame;

struct stack *
stack_create(struct frame *, struct stack *prev);

struct stack *
stack_getframe(struct stack *, int frame);

char *
stack_programtext(struct stack *);

int
stack_programindex(struct stack *);

void
stack_break(struct stack *);

void
stack_return(struct stack *);

struct ast_type *
stack_returntype(struct stack *);

struct ast_expr *
stack_framecall(struct stack *);

struct externals;

char *
stack_argmodulator(struct stack *, struct state *);

void
stack_destroy(struct stack *);

struct stack *
stack_copy(struct stack *);

struct stack *
stack_copywithname(struct stack *, char *new_name);

char *
stack_funcname(struct stack *);

char *
stack_str(struct stack *, struct state *);

bool
stack_islinear(struct stack *);

int
stack_modecanverify(struct stack *);

int
stack_modecanrunxr0cmd(struct stack *);

struct lexememarker *
stack_lexememarker(struct stack *);

int
stack_atend(struct stack *);

int
stack_atsetupend(struct stack *);

int
stack_atinvariantend(struct stack *);

int
stack_atloopend(struct stack *);

int
stack_id(struct stack *);

struct error *
stack_step(struct stack *, struct state *);

struct error *
stack_next(struct stack *, struct state *);

void
stack_nextstmt(struct stack *s, struct state *state);

struct stack *
stack_prev(struct stack *);

void
stack_popprep(struct stack *, struct state *);

void
stack_storeloc(struct stack *);

void
stack_declare(struct stack *, struct ast_variable *var, bool isparam);

void
stack_undeclare(struct stack *stack, struct state *state);

bool
stack_isnested(struct stack *);

int
stack_insetup(struct stack *);

int
stack_ininvariant(struct stack *);

int
stack_inloop(struct stack *);

int
stack_isloopbase(struct stack *);

struct error *
stack_trace(struct stack *, struct error *);

struct map *
stack_getvarmap(struct stack *);

struct variable *
stack_getvariable(struct stack *s, char *id);

struct error *
stack_constraintverify_all(struct stack *spec_stack, struct state *spec,
		struct state *impl);

struct error *
stack_verifyinvariant(struct stack *s, struct state *impl);

struct ast_expr;

struct block *
stack_getblock(struct stack *, int address);

struct frame *
frame_invariant_create(struct ast_block *, struct stack *);

struct frame *
frame_loop_create(struct ast_block *, struct stack *, struct state *inv_state);

/* variable */

struct ast_type;

struct variable;

struct stack;

struct variable *
variable_create(struct ast_type *type, struct stack *, bool isparam);

void
variable_destroy(struct variable *);

struct variable *
variable_copy(struct variable *);

struct location *
variable_location(struct variable *);

struct ast_type *
variable_type(struct variable *);

bool
variable_references(struct variable *v, struct location *loc, struct state *s);

bool
variable_isparam(struct variable *);

#endif
