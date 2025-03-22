#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "command.h"
#include "command_deque.h"

struct _node {
	struct command *_;
	struct _node *next;
	struct _node *prev;
};

struct _node *
_create_node(struct command *c)
{
	struct _node *n = malloc(sizeof(struct _node));
	assert(n);
	n->_ = command_copy(c);
	n->next = NULL;
	n->prev = NULL;
	return n;
}

void
_destroy_node(struct _node *n)
{
	command_destroy(n->_);
	free(n);
}

struct command_deque {
	struct _node *back;
	struct _node *front;
};

struct command_deque *
command_deque_create(void)
{
	struct command_deque *q = malloc(sizeof(struct command_deque));
	assert(q);
	q->back = NULL;
	q->front = NULL;
	return q;
}

struct command_deque *
command_deque_copy(struct command_deque *old)
{
	struct command_deque *new = command_deque_create();
	struct _node *n = old->front;

	while (n) {
		command_deque_pushback(new, command_copy(n->_));
		n = n->next;
	}
	return new;
}

void
command_deque_destroy(struct command_deque *dq)
{
	assert(0);
}

void
command_deque_print(struct command_deque *dq)
{
	struct _node *n = dq->front;
	while (n) {
		char *c = command_str(n->_);
		printf("%s ", c);
		free(c);
		n = n->next;
	}
	printf("\n");
}

int
command_deque_isempty(struct command_deque *dq)
{
	return dq->front == NULL;
}

struct command;

void
command_deque_pushback(struct command_deque *dq, struct command *c)
{
	struct _node *n = _create_node(c);
	n->next = NULL;
	n->prev = dq->back;

	if (command_deque_isempty(dq)) {
		dq->front = n;
	} else {
		dq->back->next = n;
	}

	dq->back = n;
}

struct command *
command_deque_popfront(struct command_deque *dq)
{
	if (command_deque_isempty(dq)) {
		return NULL;
	}
	struct _node *n = dq->front;
	struct command *c = command_copy(n->_);

	dq->front = n->next;
	if (dq->front) {
		dq->front->prev = NULL;
	} else {
		dq->back = NULL;
	}
	_destroy_node(n);

	return c;
}

struct command *
command_deque_popback(struct command_deque *dq)
{
	if (command_deque_isempty(dq)) {
		return NULL;
	}
	struct _node *n = dq->back;
	struct command *c = command_copy(n->_);

	dq->back = n->prev;
	if (dq->back) {
		dq->back->next = NULL;
	} else {
		dq->front = NULL;
	}
	_destroy_node(n);

	return c;
}
