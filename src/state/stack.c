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
	char *name;
	struct block_arr *frame;

	/* lvalues of blocks in frame */
	struct map *varmap;
	struct variable *result;

	struct stack *prev;
};

struct location *
stack_newblock(struct stack *stack)
{
	int address = block_arr_append(stack->frame, block_create());
	return location_create(
		LOCATION_AUTOMATIC, address, ast_expr_constant_create(0)
	);
}

struct stack *
stack_create(char *name, struct stack *prev, struct ast_type *result_type)
{
	struct stack *stack = calloc(1, sizeof(struct stack));
	assert(stack);

	stack->name = name;
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

struct stack *
stack_prev(struct stack *s)
{
	return s->prev;
}

static struct map *
varmap_copy(struct map *);

struct stack *
stack_copy(struct stack *stack)
{
	struct stack *copy = calloc(1, sizeof(struct stack));
	copy->name = dynamic_str(stack->name);
	copy->frame = block_arr_copy(stack->frame);
	copy->varmap = varmap_copy(stack->varmap);
	copy->result = variable_copy(stack->result);
	if (stack->prev) {
		copy->prev = stack_copy(stack->prev);
	}
	return copy;
}

static struct map *
varmap_copy(struct map *m)
{
	struct map *m_copy = map_create();
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(
			m_copy,
			dynamic_str(e.key),
			variable_copy((struct variable *) e.value)
		);
	}
	return m_copy;
}

char *
stack_str(struct stack *stack, struct state *state)
{
	struct strbuilder *b = strbuilder_create();
	struct map *m = stack->varmap;
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		char *var = variable_str((struct variable *) e.value, stack, state);
		strbuilder_printf(b, "\t%s: %s", e.key, var);
		free(var);
		strbuilder_putc(b, '\n');
	}
	char *result = variable_str(stack->result, stack, state);
	strbuilder_printf(b, "\tresult: %s\n", result);
	free(result);
	strbuilder_printf(b, "\t");
	/* TODO: fix length of line */
	for (int i = 0, len = 30; i < len-2; i++ ) {
		strbuilder_putc(b, '-');
	}
	strbuilder_printf(b, " %s\n", stack->name);
	if (stack->prev) {
		char *prev = stack_str(stack->prev, state);
		strbuilder_printf(b, prev);
		free(prev);
	}
	return strbuilder_build(b);
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

void
stack_undeclare(struct stack *stack)
{
	struct map *m = stack->varmap;
	stack->varmap = map_create();
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		struct variable *v = (struct variable *) e.value;
		if (variable_isparam(v)) {
			map_set(stack->varmap, dynamic_str(e.key), v);
		} else {
			variable_destroy(v);
		}
	}
	map_destroy(m);
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

struct variable *
stack_getvariable(struct stack *s, char *id)
{
	assert(strcmp(id, KEYWORD_RESULT) != 0);

	return map_get(s->varmap, id);
}

bool
stack_references(struct stack *s, struct location *loc, struct state *state)
{
	/* TODO: check globals */
	struct variable *result = stack_getresult(s);
	if (result && variable_references(result, loc, state)) {
		return true;
	}

	struct map *m = s->varmap;
	for (int i = 0; i < m->n; i++) {
		struct variable *var = (struct variable *) m->entry[i].value;
		if (variable_isparam(var) && variable_references(var, loc, state)) {
			return true;
		}
	}

	return false;
}

struct block *
stack_getblock(struct stack *s, int address)
{
	assert(address < block_arr_nblocks(s->frame));

	return block_arr_blocks(s->frame)[address];
}


struct variable {
	struct ast_type *type;
	struct location *loc;
	bool isparam;
};

struct variable *
variable_create(struct ast_type *type, struct stack *stack, bool isparam)
{
	struct variable *v = malloc(sizeof(struct variable));

	v->type = ast_type_copy(type);
	v->isparam = isparam;

	/* create block with uninitialised object at offset 0 */
	v->loc = stack_newblock(stack);
	struct block *b = location_getblock(v->loc, NULL, stack, NULL);
	assert(b);
	block_install(b, object_value_create(ast_expr_constant_create(0), NULL));

	return v;
}

void
variable_destroy(struct variable *v)
{
	ast_type_destroy(v->type);
	location_destroy(v->loc);
	free(v);
}

struct variable *
variable_copy(struct variable *v)
{
	struct variable *copy = malloc(sizeof(struct variable));
	copy->type = ast_type_copy(v->type);
	copy->loc = location_copy(v->loc);
	copy->isparam = v->isparam;
	return copy;
}

static char *
object_or_nothing_str(struct location *loc, struct stack *stack, struct state *state);

char *
variable_str(struct variable *var, struct stack *stack, struct state *state)
{
	assert(location_type(var->loc) != LOCATION_VCONST);

	struct strbuilder *b = strbuilder_create();
	char *type = ast_type_str(var->type);
	char *loc = location_str(var->loc);
	char *isparam = var->isparam ? "param " : "";
	char *obj_str = object_or_nothing_str(var->loc, stack, state);
	strbuilder_printf(b, "{%s%s := %s} @ %s", isparam, type, obj_str, loc);
	free(obj_str);
	free(loc);
	free(type);
	return strbuilder_build(b);
}

static char *
object_or_nothing_str(struct location *loc, struct stack *stack, struct state *state)
{
	struct block *b = location_getstackblock(loc, stack);
	assert(b);
	struct object *obj = block_observe(b, location_offset(loc), state, false);
	if (obj) {
		return object_str(obj);
	}
	return dynamic_str("");
}

struct location *
variable_location(struct variable *v)
{
	return v->loc;
}

struct ast_type *
variable_type(struct variable *v)
{
	return v->type;
}

bool
variable_references(struct variable *v, struct location *loc, struct state *s)
{
	assert(location_type(loc) != LOCATION_VCONST);

	return location_references(v->loc, loc, s);
}

bool
variable_isparam(struct variable *v)
{
	return v->isparam;
}
