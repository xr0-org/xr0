#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "location.h"
#include "block.h"
#include "heap.h"
#include "state.h"
#include "stack.h"
#include "object.h"
#include "value.h"
#include "util.h"

struct stack {
	struct block_arr *frame;

	/* lvalues of blocks in frame */
	struct map *varmap;
	struct variable *result;

	struct stack *prev;
};

struct location *
stack_newblock(struct stack *stack, int size)
{
	int address = block_arr_append(stack->frame, block_create(size));
	return location_create(
		LOCATION_AUTOMATIC, address, ast_expr_create_constant(0)
	);
}

struct stack *
stack_create(struct stack *prev, struct ast_type *result_type)
{
	struct stack *stack = calloc(1, sizeof(struct stack));
	assert(stack);

	stack->frame = block_arr_create();

	stack->varmap = map_create();

	stack->result = variable_create(result_type, stack, false);

	stack->prev = prev;

	return stack;
}

void
stack_destroy(struct stack *stack)
{
	block_arr_destroy(stack->frame);

	struct map *m = stack->varmap;
	for (int i = 0; i < m->n; i++) {
		variable_destroy((struct variable *) m->entry[i].value);
	}
	map_destroy(m);

	variable_destroy(stack->result);

	free(stack);
}

char *
stack_str(struct stack *stack, struct heap *h)
{
	struct strbuilder *b = strbuilder_create();
	struct map *m = stack->varmap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		char *var = variable_str((struct variable *) e.value, stack, h);
		strbuilder_printf(b, "\t%s: %s", e.key, var);
		free(var);
		strbuilder_putc(b, '\n');
	}
	char *result = variable_str(stack->result, stack, h);
	strbuilder_printf(b, "\tresult: %s\n", result);
	free(result);
	if (stack->prev) {
		strbuilder_printf(b, "\t");
		char *prev = stack_str(stack->prev, h);
		/* TODO: fix length of line */
		for (int i = 0, len = 30; i < len-2; i++ ) {
			strbuilder_putc(b, '-');
		}
		strbuilder_printf(b, "\n");
		strbuilder_printf(b, prev);
		free(prev);
	}
	return strbuilder_build(b);
}

struct stack *
stack_prev(struct stack *stack, int depth)
{
	assert(!depth || stack->prev);

	return depth > 0 ? stack_prev(stack->prev, depth-1) : stack;
}

void
stack_declare(struct stack *stack, struct ast_variable *var, bool isparam)
{
	char *id = ast_variable_name(var);
	assert(!map_get(stack->varmap, id));
	map_set(
		stack->varmap,
		dynamic_str(id),
		variable_create(ast_variable_type(var), stack, isparam)
	);
}

struct ast_variable **
stack_getvariables(struct stack *stack)
{
	struct map *m = stack->varmap;

	struct ast_variable **var = malloc(sizeof(struct ast_variable *) * m->n);
	assert(var);
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];

		struct variable *v = (struct variable *) e.value;
		assert(v);

		var[i] = variable_to_ast(v, e.key);
	}
	return var;
}

int
stack_nvariables(struct stack *stack)
{
	return stack->varmap->n;
}


struct variable *
stack_getresult(struct stack *s)
{
	return s->result;
}

struct map *
stack_getvarmap(struct stack *s)
{
	return s->varmap;
}

static struct location *
stack_getlocation_prop(struct stack *s, char *id, int currdepth);

struct location *
stack_getlocation(struct stack *s, char *id)
{
	if (strcmp(id, KEYWORD_RESULT) == 0) {
		return location_copy(variable_location(s->result));
	}
	return stack_getlocation_prop(s, id, 0);
}

static struct location *
stack_getlocation_prop(struct stack *s, char *id, int depth)
{
	/* XXX */
	struct variable *v = map_get(s->varmap, id);
	if (v) {
		struct location *loc = location_copy(variable_location(v));
		location_setdepth(loc, depth);
		return loc;
	}
	return s->prev ? stack_getlocation_prop(s->prev, id, depth+1) : NULL;
}

bool
stack_references(struct stack *s, struct heap *h, struct location *loc)
{
	/* TODO: check globals */

	if (variable_references(stack_getresult(s), loc, s, h)) {
		return true;
	}

	struct map *m = s->varmap;
	for (int i = 0; i < m->n; i++) {
		struct variable *v = (struct variable *) m->entry[i].value;
		if (variable_isparam(v) && variable_references(v, loc, s, h)) {
			return true;
		}
	}

	return false;
}

block *
stack_getblock(struct stack *s, int address)
{
	assert(address < block_arr_nblocks(s->frame));

	return block_arr_blocks(s->frame)[address];
}
