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
stack_newblock(struct stack *stack, int size)
{
	int address = block_arr_append(stack->memory, block_create(size));
	struct location *loc = location_create_automatic(
		stack->id, address, offset_create(ast_expr_constant_create(0))
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
	switch (s->kind) {
	case FRAME_CALL:
		assert(s->call);
		return s->call;
	case FRAME_INTERMEDIATE:
		return program_prevcall(s->p);
	default:
		assert(false);
	}
	return NULL;
}

static char *
argmodulator(struct stack *, struct state *);

char *
stack_argmodulator(struct stack *stack, struct state *state)
{
	if (!stack->prev) {
		/* base frame */
		assert(stack->kind == FRAME_CALL);
		return dynamic_str("");
	}
	switch (stack->kind) {
	case FRAME_CALL:
		assert(stack->call);
		return argmodulator(stack, state);
	case FRAME_NESTED:
	case FRAME_INTERMEDIATE:
	case FRAME_SETUP:
		assert(stack->prev);
		return stack_argmodulator(stack->prev, state);
	default:
		assert(false);
	}
}

static char *
argmodulator(struct stack *stack, struct state *state)
{
	struct string_arr *arr = string_arr_create();
	struct map *m = stack->varmap;
	for (int i = 0; i < m->n; i++) {
		struct variable *v = (struct variable *) m->entry[i].value;
		if (!variable_isparam(v)) {
			continue;
		}
		struct location *loc = variable_location(v);
		assert(loc);
		string_arr_append(
			arr,
			value_str(
				object_as_value(
					object_res_as_object(
						state_get(state, loc, false)
					)
				)
			)
		);
	}
	char *ret = string_arr_str(arr);
	string_arr_destroy(arr);
	return ret;
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
	copy->name = dynamic_str(stack->name);
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

static struct string_arr *
var_strs(struct stack *, struct state *);

static int
maxlenorzero(struct string_arr *);

char *
stack_str(struct stack *stack, struct state *state)
{
	struct strbuilder *b = strbuilder_create();
	struct map *m = stack->varmap;
	struct string_arr *var_str_arr = var_strs(stack, state);
	int var_str_max = maxlenorzero(var_str_arr);
	assert(string_arr_n(var_str_arr) == m->n);
	char **var_str = string_arr_s(var_str_arr);

	for (int i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		struct variable *v = (struct variable *) e.value;
		char *decl = ast_type_strwithvar(variable_type(v), e.key);
		strbuilder_printf(
			b, 
			"\t%d: %-*s (%s",
			i,
			var_str_max, var_str[i],
			decl
		);
		if (variable_isparam(v)) {
			strbuilder_printf(b, ", π");
		}
		strbuilder_printf(b, ")\n");
		free(decl);
	}
	string_arr_destroy(var_str_arr);
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


static struct string_arr *
var_strs(struct stack *stack, struct state *state)
{
	struct string_arr *arr = string_arr_create();
	struct map *m = stack->varmap;
	for (int i = 0; i < m->n; i++) {
		struct block *b = location_getstackblock(
			variable_location((struct variable *) m->entry[i].value),
			stack
		);
		assert(b);
		string_arr_append(arr, block_str(b));
	}
	return arr;
}

static int
maxlenorzero(struct string_arr *arr)
{
	int n = string_arr_n(arr);
	char **s = string_arr_s(arr);
	int max = 0;
	for (int i = 0; i < n; i++) {
		int len = strlen(s[i]);
		if (len > max) {
			max = len;
		}
	}
	return max;
}

bool
stack_islinear(struct stack *s)
{
	return s->kind == FRAME_INTERMEDIATE;
}

bool
stack_insetup(struct stack *s)
{	
	if (s->kind == FRAME_SETUP) {
		return true;
	}
	if (s->prev == NULL) {
		return false;
	}
	return stack_insetup(s->prev);
}

int
stack_id(struct stack *s)
{
	assert(s);
	return s->id;
}

enum execution_mode
stack_execmode(struct stack *s)
{
	return s->mode;
}

struct lexememarker *
stack_lexememarker(struct stack *s)
{
	return program_lexememarker(s->p);
}

static bool
stack_isbase(struct stack *s)
{
	if (s->id == 0) {
		assert(s->kind == FRAME_CALL);
		return true;
	}
	return false;
}

static bool
stack_issetupbase(struct stack *s)
{
	if (s->id == 1) { 
		assert(s->kind == FRAME_SETUP);
		return true;
	}
	return false;
}

bool
stack_atend(struct stack *s)
{
	return program_atend(s->p) && stack_isbase(s);
}

bool
stack_atsetupend(struct stack *s)
{
	return program_atend(s->p) && stack_issetupbase(s);
}

struct error *
stack_step(struct stack *s, struct state *state)
{
	return program_step(s->p, state);
}

struct error *
stack_next(struct stack *s, struct state *state)
{
	return program_next(s->p, state);
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
		assert(state_readregister(state));
	}
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
	struct frame* f = frame_create(n, b, NULL, mode, FRAME_SETUP);
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

	v->loc = stack_newblock(stack, ast_type_size(type));
	struct block_res *res = location_getblock(v->loc, NULL, NULL, stack, NULL, NULL);
	struct block *b = block_res_as_block(res);
	assert(b);

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
	struct object_res *res = state_get(s, new->loc, false);
	struct object *obj = object_res_as_object(res);
	assert(obj);
	if (object_hasvalue(obj)) {
		struct value *v = object_as_value(obj);
		if (v) {
			object_assign(obj, value_abstractcopy(v, s));
		}
	}
	return new;
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
	bool ans = location_referencescaller(v->loc, loc, s, cb);
	circuitbreaker_destroy(cb);
	return ans;
}

bool
variable_isparam(struct variable *v)
{
	return v->isparam;
}
