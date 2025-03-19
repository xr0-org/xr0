#ifndef COMMAND_H
#define COMMAND_H

struct verifier;

/* command */

struct command;

struct command *
command_read(char *debugsep);

struct error *
command_exec(struct verifier *, struct command *, char *debugsep);

struct command *
command_copy(struct command *);

/* command_stack */

struct command_stack;

struct command_stack *
command_stack_create(void);

struct command_stack *
command_stack_copy(struct command_stack *);

int
command_stack_isempty(struct command_stack *);

void
command_stack_push(struct command_stack *, struct command *);

struct command *
command_stack_pop(struct command_stack *);

void
command_stack_destroy(struct command_stack *);

#endif
