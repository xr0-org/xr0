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
#include "program.h"

struct frame;

struct stack {
	struct program *pc;
	bool abstract;

	struct block_arr *memory;

	/* lvalues of blocks in frame */
	struct map *varmap;
	struct variable *result;

	int id;
	struct stack *prev;
};

struct location *
stack_newblock(struct stack *stack)
{
	int address = block_arr_append(stack->memory, block_create());
	struct location *loc = location_create_automatic(
		stack->id, address, ast_expr_constant_create(0)
	);
	return loc;
}

struct stack *
stack_create(char *name, struct ast_block *b, struct ast_type *ret_type,
		bool abstract, struct stack *prev)
{
	struct stack *stack = calloc(1, sizeof(struct stack));
	assert(stack);

	assert(b);
	stack->pc = program_create(b, name);
	stack->abstract = abstract;
	stack->memory = block_arr_create();

	stack->varmap = map_create();

	stack->prev = prev;
	stack->id = prev ? prev->id + 1 : 0;

	stack->result = variable_create(ret_type, stack, false);

	return stack;
}

struct stack *
stack_getframe(struct stack *s, int frame)
{
	assert(s);
	assert(frame >= 0);

	if (s->id == frame) {
		return s;
	}
	if (!s->prev) {
		return NULL;
	}
	return stack_getframe(s->prev, frame);
}

void
stack_destroy(struct stack *stack)
{
	block_arr_destroy(stack->memory);

	struct map *m = stack->varmap;
	for (int i = 0; i < m->n; i++) {
		variable_destroy((struct variable *) m->entry[i].value);
	}
	map_destroy(m);

	variable_destroy(stack->result);

	program_destroy(stack->pc);
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
	copy->abstract = stack->abstract;
	copy->pc = program_copy(stack->pc);
	copy->memory = block_arr_copy(stack->memory);
	copy->varmap = varmap_copy(stack->varmap);
	copy->id = stack->id;
	copy->result = variable_copy(stack->result);
	if (stack->prev) {
		copy->prev = stack_copy(stack->prev);
	}
	return copy;
}

struct stack *
stack_copywithname(struct stack *stack, char *new_name)
{
	struct stack *copy = stack_copy(stack);
	program_changename(copy->pc, new_name);
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
	strbuilder_printf(b, "\treturn: %s\n", result);
	free(result);
	strbuilder_printf(b, "\t");
	/* TODO: fix length of line */
	for (int i = 0, len = 30; i < len-2; i++ ) {
		strbuilder_putc(b, '-');
	}
	strbuilder_printf(b, " %s\n", program_name(stack->pc));
	if (stack->prev) {
		char *prev = stack_str(stack->prev, state);
		strbuilder_printf(b, prev);
		free(prev);
	}
	return strbuilder_build(b);
}

bool
stack_atend(struct stack *s)
{
	return !s->prev && program_atend(s->pc);
}

struct error *
stack_step(struct stack *s, struct state *state)
{
	return program_exec(s->pc, s->abstract, state);
}

void
stack_declare(struct stack *stack, struct ast_variable *var, bool isparam)
{
	char *id = ast_variable_name(var);
	assert(!map_get(stack->varmap, id)); /* XXX: user error */
	map_set(
		stack->varmap,
		dynamic_str(id),
		variable_create(ast_variable_type(var), stack, isparam)
	);
}

static struct variable *
variable_abstractcopy(struct variable *v, struct state *s);

void
stack_undeclare(struct stack *stack, struct state *state)
{
	struct variable *old_result = stack->result;
	stack->result = variable_abstractcopy(old_result, state);
	variable_destroy(old_result);

	struct map *m = stack->varmap;
	stack->varmap = map_create();
	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		struct variable *v = (struct variable *) e.value;
		if (variable_isparam(v)) {
			map_set(
				stack->varmap, dynamic_str(e.key),
				variable_abstractcopy(v, state)
			);
		}
		variable_destroy(v);
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
	assert(strcmp(id, KEYWORD_RETURN) != 0);

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
	assert(address < block_arr_nblocks(s->memory));

	return block_arr_blocks(s->memory)[address];
}

struct frame {
	char *name;
	struct ast_block *b;
	struct ast_type *ret_type;
	bool abstract;
};

static struct frame *
frame_create(char *n, struct ast_block *b, struct ast_type *r, bool abs)
{
	struct frame *f = malloc(sizeof(struct frame));
	f->name = n;
	f->b = b;
	f->ret_type = r;
	f->abstract = abs;
	return f;
}

struct frame *
frame_call_create(char *n, struct ast_block *b, struct ast_type *r, bool abs)
{
	return frame_create(n, b, r, abs);
}

struct frame *
frame_block_create(char *n, struct ast_block *b)
{
	return frame_create(n, b, NULL, false);
}

struct frame *
frame_copy(struct frame *f)
{
	return frame_create(
		dynamic_str(f->name),
		ast_block_copy(f->b),
		ast_type_copy(f->ret_type),
		f->abstract
	); 
}

static void
frame_destroy(struct frame *f)
{
	assert(false);
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
	struct block_res res = location_getblock(v->loc, NULL, NULL, stack, NULL, NULL);
	if (res.err) {
		assert(false);
	}
	assert(res.b);
	block_install(res.b, object_value_create(ast_expr_constant_create(0), NULL));

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
variable_copy(struct variable *old)
{
	struct variable *new = malloc(sizeof(struct variable));
	new->type = ast_type_copy(old->type);
	new->isparam = old->isparam;
	new->loc = location_copy(old->loc);
	return new;
}

static struct variable *
variable_abstractcopy(struct variable *old, struct state *s)
{
	struct variable *new = malloc(sizeof(struct variable));
	new->type = ast_type_copy(old->type);
	new->isparam = old->isparam;
	new->loc = location_copy(old->loc);
	struct object_res res = state_get(s, new->loc, false);
	if (res.err) {
		assert(false);
	}
	assert(res.obj);
	if (object_isvalue(res.obj)) {
		struct value *v = object_as_value(res.obj);
		if (v) {
			object_assign(res.obj, value_abstractcopy(v, s));
		}
	}
	return new;
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

	struct circuitbreaker *cb = circuitbreaker_create();
	bool ans = location_references(v->loc, loc, s, cb);
	circuitbreaker_destroy(cb);
	return ans;
}

bool
variable_isparam(struct variable *v)
{
	return v->isparam;
}
