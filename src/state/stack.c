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
	int id;
	struct program *p;
	struct block_arr *memory;
	struct map *varmap;		/* lvalues of blocks in frame */
	struct stack *prev;

	char *name;
	enum execution_mode mode;
	enum frame_kind kind;
	struct ast_expr *call;
	struct ast_function *f;
};

struct frame {
	char *name;
	struct ast_block *b;
	struct ast_type *ret_type;
	enum execution_mode mode;
	enum frame_kind kind;
	struct ast_expr *call;
	struct ast_function *f;
	bool advance;
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
	stack->p = program_create(f->b);
	stack->memory = block_arr_create();
	stack->varmap = map_create();
	stack->prev = prev;
	stack->id = prev ? prev->id + 1 : 0;

	stack->name = f->name;
	stack->mode = f->mode;
	stack->kind = f->kind;
	if (stack->kind == FRAME_CALL) {
		stack->call = f->call;
		stack->f = f->f;
	}

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

char *
stack_programtext(struct stack *s)
{
	return program_render(s->p);
}

int
stack_programindex(struct stack *s)
{
	return program_index(s->p);
}

void
stack_return(struct stack *s)
{
	program_setatend(s->p);
	if (s->prev && s->kind != FRAME_CALL) {
		stack_return(s->prev);
	}
}

struct ast_expr *
stack_framecall(struct stack *s)
{
	if (s->kind == FRAME_CALL) {
		assert(s->call);
		return s->call;
	}
	return NULL;
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

	/* XXX: call expr leak */

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
	copy->mode = stack->mode;
	copy->p = program_copy(stack->p);
	copy->memory = block_arr_copy(stack->memory);
	copy->varmap = varmap_copy(stack->varmap);
	copy->id = stack->id;
	if (stack->prev) {
		copy->prev = stack_copy(stack->prev);
	}
	copy->kind = stack->kind;
	if (stack->kind == FRAME_CALL) {
		copy->call = ast_expr_copy(stack->call);
		copy->f = ast_function_copy(stack->f);
	}
	return copy;
}

static void
rename_lowestframe(struct stack *, char *new_name);

struct stack *
stack_copywithname(struct stack *stack, char *new_name)
{
	struct stack *copy = stack_copy(stack);
	rename_lowestframe(copy, new_name);
	return copy;
}

static void
rename_lowestframe(struct stack *s, char *new_name)
{
	if (s->prev) {
		rename_lowestframe(s->prev, new_name);
	} else {
		s->name = new_name;
	}
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

bool
stack_islinear(struct stack *s)
{
	return s->kind == FRAME_INTERMEDIATE;
}

enum execution_mode
stack_execmode(struct stack *s)
{
	return s->mode;
}

bool
stack_atend(struct stack *s)
{
	return program_atend(s->p);
}

struct error *
stack_step(struct stack *s, struct state *state)
{
	return program_exec(s->p, s->mode, state);
}

void
stack_storeloc(struct stack *s)
{
	program_storeloc(s->p);
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
	if (s->prev) {
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
	char *msg = strbuilder_build(b);
	struct error *e = error_printf("%s", msg);
	free(msg);
	return e;
}

static char *
stack_propername(struct stack *s)
{
	return s->kind == FRAME_CALL ? s->name : stack_propername(s->prev);
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

void
stack_popprep(struct stack *s, struct state *state)
{
	if (s->kind == FRAME_CALL && !ast_function_isvoid(s->f)) {
		struct value *v = state_readregister(state);
		if (!v) {
			state_writeregister(
				state,
				ast_expr_call_arbitrary(s->call, s->f, state)
			);
		}
	}
}

bool
stack_references(struct stack *s, struct location *loc, struct state *state)
{
	/* TODO: check globals */
	struct value *result = state_popregister(state);
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
frame_create(char *n, struct ast_block *b, struct ast_type *r, enum execution_mode mode,
		enum frame_kind kind)
{
	struct frame *f = malloc(sizeof(struct frame));
	f->name = dynamic_str(n);
	f->b = ast_block_copy(b);
	if (kind == FRAME_CALL) {
		assert(r);
		f->ret_type = ast_type_copy(r);
	}
	f->mode = mode;
	f->kind = kind;
	f->call = NULL;
	f->f = NULL;
	f->advance = true;
	return f;
}

struct frame *
frame_call_create(char *n, struct ast_block *b, struct ast_type *r, enum execution_mode mode, struct ast_expr *call, struct ast_function *func)
{
	struct frame *f = frame_create(n, b, r, mode, FRAME_CALL);
	f->call = call;
	f->f = func;
	return f;
}

struct frame *
frame_block_create(char *n, struct ast_block *b, enum execution_mode mode)
{
	return frame_create(n, b, NULL, mode, FRAME_NESTED);
}

struct frame *
frame_setup_create(char *n, struct ast_block *b, enum execution_mode mode)
{
	struct frame* f = frame_create(n, b, NULL, mode, FRAME_NESTED);
	f->advance = false;
	return f;
}

struct frame *
frame_intermediate_create(char *n, struct ast_block *b, enum execution_mode mode)
{
	return frame_create(n, b, NULL, mode, FRAME_INTERMEDIATE);
}

bool
frame_advance(struct frame *f)
{
	return f->advance;
}

static struct frame *
frame_copy(struct frame *f)
{
	if (f->kind == FRAME_CALL) {
		assert(f->ret_type);
	}
	struct frame *copy = frame_create(
		dynamic_str(f->name),
		ast_block_copy(f->b),
		f->ret_type ? ast_type_copy(f->ret_type) : NULL,
		f->mode,
		f->kind
	); 
	if (f->kind == FRAME_CALL) {
		copy->f = ast_function_copy(f->f);
		copy->call = ast_expr_copy(f->call);
	}
	copy->advance = f->advance;
	return copy;
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
