#ifndef COMMAND_DEQUE_H
#define COMMAND_DEQUE_H

struct command_deque;

struct command_deque *
command_deque_create(void);

struct command_deque *
command_deque_copy(struct command_deque *);

void
command_deque_destroy(struct command_deque *);

int
command_deque_isempty(struct command_deque *);

struct command;

void
command_deque_pushback(struct command_deque *, struct command *);

struct command *
command_deque_popfront(struct command_deque *);

struct command *
command_deque_popback(struct command_deque *);

void
command_deque_print(struct command_deque *);

#endif
