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
	bool abstract;
	struct program *p;

	struct block_arr *memory;

	/* lvalues of blocks in frame */
	struct map *varmap;
	struct variable *result;

	int id;
	struct stack *prev;
	enum frame_kind kind;
};

struct frame {
	char *name;
	struct ast_block *b;
	struct ast_type *ret_type;
	bool abstract;
	enum frame_kind kind;
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
stack_create(struct frame *f, struct stack *prev)
{
	struct stack *stack = calloc(1, sizeof(struct stack));
	assert(stack);

	assert(f->b);
	stack->p = program_create(f->b, f->name);
	stack->abstract = f->abstract;
	stack->memory = block_arr_create();

	stack->varmap = map_create();

	stack->prev = prev;
	stack->id = prev ? prev->id + 1 : 0;

	stack->kind = f->kind;
	stack->result = stack->kind != FRAME_CALL
		? NULL : variable_create(f->ret_type, stack, false);

	return stack;
}

int
stack_id(struct stack *s)
{
	assert(s);
	return s->id;
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

	if (stack->result) {
		variable_destroy(stack->result);
	}

	program_destroy(stack->p);
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
	copy->p = program_copy(stack->p);
	copy->memory = block_arr_copy(stack->memory);
	copy->varmap = varmap_copy(stack->varmap);
	copy->id = stack->id;
	copy->result = stack->result
		? variable_copy(stack->result) : NULL;
	if (stack->prev) {
		copy->prev = stack_copy(stack->prev);
	}
	copy->kind = stack->kind;
	return copy;
}

struct stack *
stack_copywithname(struct stack *stack, char *new_name)
{
	struct stack *copy = stack_copy(stack);
	program_changename(copy->p, new_name);
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
	if (stack->result) {
		char *result = variable_str(stack->result, stack, state);
		strbuilder_printf(b, "\treturn: %s\n", result);
		free(result);
	}
	strbuilder_printf(b, "\t");
	/* TODO: fix length of line */
	for (int i = 0, len = 30; i < len-2; i++ ) {
		strbuilder_putc(b, '-');
	}
	strbuilder_printf(b, " %s\n", program_name(stack->p));
	if (stack->prev) {
		char *prev = stack_str(stack->prev, state);
		strbuilder_printf(b, prev);
		free(prev);
	}
	return strbuilder_build(b);
}

bool
stack_linear(struct stack *s)
{
	return s->kind == FRAME_INTERMEDIATE;
}

bool
stack_atend(struct stack *s)
{
	return program_atend(s->p);
}

struct error *
stack_step(struct stack *s, struct state *state)
{
	return program_exec(s->p, s->abstract, state);
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
	if (stack->result) {
		struct variable *old_result = stack->result;
		assert(old_result);
		stack->result = variable_abstractcopy(old_result, state);
		variable_destroy(old_result);
	}

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

static bool
stack_iscall(struct stack *s)
{
	return s->kind == FRAME_CALL;
}

bool
stack_isnested(struct stack *s)
{
	return s->kind == FRAME_NESTED;
}

static char *
stack_propername(struct stack *);

static char *
stack_context(struct stack *);

struct error *
stack_trace(struct stack *s, struct error *err)
{
	if (s->kind != FRAME_CALL) {
		assert(s->prev);
		return stack_trace(s->prev, err);
	}

	struct strbuilder *b = strbuilder_create();

	char *loc = program_loc(s->p);
	char *err_str = error_str(err);
	strbuilder_printf(b, "%s: %s (%s)", loc, err_str, stack_propername(s));
	free(err_str);
	free(loc);
	char *context = stack_context(s->prev);
	if (strlen(context)) {
		strbuilder_printf(b, "%s", context);
	}
	free(context);

	char *msg = strbuilder_build(b);
	struct error *trace_err = error_printf("%s", msg);
	free(msg);
	return trace_err;
}

static char *
stack_propername(struct stack *s)
{
	return s->kind == FRAME_CALL ? program_name(s->p) : stack_propername(s->prev);
}

static char *
stack_context(struct stack *s)
{
	struct strbuilder *b = strbuilder_create();
	if (s->kind == FRAME_CALL) {
		char *loc = program_loc(s->p);
		strbuilder_printf(b, "\t%s (%s)", loc, stack_propername(s));
		free(loc);
	}
	if (s->prev) {
		char *prev = stack_context(s->prev);
		if (strlen(prev)) {
			strbuilder_printf(b, "\n%s", prev);
		}
	}
	return strbuilder_build(b);
}

struct variable *
stack_getresult(struct stack *s)
{
	if (!s->prev || s->kind == FRAME_CALL) {
		/* ⊢ lowest frame || call */
		return s->result;
	}
	/* ⊢ block || intermediate  */
	return stack_getresult(s->prev);
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

	struct variable *v = map_get(s->varmap, id);
	if (!v && !stack_iscall(s)) {
		/* ⊢ block */
		return stack_getvariable(s->prev, id);
	}
	return v;
}

bool
stack_references(struct stack *s, struct location *loc, struct state *state)
{
	/* TODO: check globals */
	struct value *result = state_readregister(state);
	if (result && value_references(result, loc, state)) {
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

static struct frame *
frame_create(char *n, struct ast_block *b, struct ast_type *r, bool abs,
		enum frame_kind kind)
{
	struct frame *f = malloc(sizeof(struct frame));
	f->name = dynamic_str(n);
	f->b = ast_block_copy(b);
	if (kind == FRAME_CALL) {
		assert(r);
		f->ret_type = ast_type_copy(r);
	}
	f->abstract = abs;
	f->kind = kind;
	return f;
}

struct frame *
frame_call_create(char *n, struct ast_block *b, struct ast_type *r, bool abs)
{
	return frame_create(n, b, r, abs, FRAME_CALL);
}

struct frame *
frame_block_create(char *n, struct ast_block *b, bool abs)
{
	return frame_create(n, b, NULL, abs, FRAME_NESTED);
}

struct frame *
frame_intermediate_create(char *n, struct ast_block *b, bool abs)
{
	return frame_create(n, b, NULL, abs, FRAME_INTERMEDIATE);
}

static struct frame *
frame_copy(struct frame *f)
{
	if (f->kind == FRAME_CALL) {
		assert(f->ret_type);
	}
	return frame_create(
		dynamic_str(f->name),
		ast_block_copy(f->b),
		f->ret_type ? ast_type_copy(f->ret_type) : NULL,
		f->abstract,
		f->kind
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
