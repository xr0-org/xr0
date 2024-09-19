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

static struct frame *
frame_copy(struct frame *);

static void
frame_destroy(struct frame *);

static void
frame_setname(struct frame *, char *);

static char *
frame_name(struct frame *);

static int
frame_modecanverify(struct frame *);

static int
frame_modecanrunxr0cmd(struct frame *);

static struct program *
frame_program(struct frame *);

static int
frame_iscall(struct frame *);

static struct ast_expr *
frame_call(struct frame *);

static struct ast_function *
frame_function(struct frame *);

static int
frame_isblock(struct frame *);

static int
frame_isinter(struct frame *);

static int
frame_issetup(struct frame *);

struct stack {
	int id;

	struct block_arr *memory;
	struct map *varmap;		/* lvalues of blocks in frame */
	struct stack *prev;
	struct frame *f;
};

static struct location *
stack_newblock(struct stack *stack, int size)
{
	struct block *b = block_create(size);
	/* "Storage is guaranteed to be reserved for a new instance of such an
	 * object on each normal entry into the block in which it is declared,
	 * or on a jump from outside the block to a label in the block or in an
	 * enclosed block." (3.1.2.4) */
	block_install(b, object_value_create(ast_expr_constant_create(0), NULL));
	return location_create_automatic(
		stack->id,
		block_arr_append(stack->memory, b),
		offset_create(ast_expr_constant_create(0))
	);
}

struct stack *
stack_create(struct frame *f, struct stack *prev)
{
	struct stack *stack = calloc(1, sizeof(struct stack));
	assert(stack);

	stack->memory = block_arr_create();
	stack->varmap = map_create();
	stack->prev = prev;
	stack->id = prev ? prev->id + 1 : 0;

	stack->f = f;

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
	return program_render(frame_program(s->f));
}

int
stack_programindex(struct stack *s)
{
	return program_index(frame_program(s->f));
}

void
stack_return(struct stack *s)
{
	program_setatend(frame_program(s->f));
	if (s->prev && !frame_iscall(s->f)) {
		stack_return(s->prev);
	}
}

struct ast_expr *
stack_framecall(struct stack *s)
{
	return frame_call(s->f);
}

struct ast_type *
stack_returntype(struct stack *s)
{
	return ast_function_type(frame_function(s->f));
}

static char *
argmodulator(struct stack *, struct state *);

char *
stack_argmodulator(struct stack *stack, struct state *state)
{
	if (!stack->prev) {
		/* base frame */
		assert(frame_iscall(stack->f));
		return dynamic_str("");
	}
	if (frame_iscall(stack->f)) {
		assert(frame_call(stack->f));
		return argmodulator(stack, state);
	}
	return stack_argmodulator(stack->prev, state);
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

	frame_destroy(stack->f);
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
	copy->memory = block_arr_copy(stack->memory);
	copy->varmap = varmap_copy(stack->varmap);
	copy->id = stack->id;
	if (stack->prev) {
		copy->prev = stack_copy(stack->prev);
	}
	copy->f = frame_copy(stack->f);
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
		frame_setname(s->f, new_name);
	}
}

char *
stack_funcname(struct stack *s)
{
	return s->prev ? stack_funcname(s->prev) : frame_name(s->f);
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
	strbuilder_printf(b, " %s\n", frame_name(stack->f));
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
	return frame_isinter(s->f);
}

bool
stack_insetup(struct stack *s)
{
	return frame_issetup(s->f) || (s->prev && stack_insetup(s->prev));
}

int
stack_id(struct stack *s)
{
	assert(s);
	return s->id;
}

int
stack_modecanverify(struct stack *s)
{
	return frame_modecanverify(s->f);
}

int
stack_modecanrunxr0cmd(struct stack *s)
{
	return frame_modecanrunxr0cmd(s->f);
}


struct lexememarker *
stack_lexememarker(struct stack *s)
{
	return program_lexememarker(frame_program(s->f));
}

static bool
stack_isbase(struct stack *s)
{
	if (s->id == 0) {
		assert(frame_iscall(s->f));
		return true;
	}
	return false;
}

static bool
stack_issetupbase(struct stack *s)
{
	if (s->id == 1) { 
		assert(frame_issetup(s->f) || frame_isblock(s->f));
		return true;
	}
	return false;
}

bool
stack_atend(struct stack *s)
{
	return program_atend(frame_program(s->f)) && stack_isbase(s);
}

bool
stack_atsetupend(struct stack *s)
{
	return program_atend(frame_program(s->f)) && stack_issetupbase(s);
}

struct error *
stack_step(struct stack *s, struct state *state)
{
	return program_step(frame_program(s->f), state);
}

struct error *
stack_next(struct stack *s, struct state *state)
{
	return program_next(frame_program(s->f), state);
}

void
stack_storeloc(struct stack *s)
{
	program_storeloc(frame_program(s->f));
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

bool
stack_isnested(struct stack *s)
{
	return frame_isblock(s->f);
}

char *
error_context(struct stack *);

static char *
stack_propername(struct stack *);

struct error *
stack_trace(struct stack *s, struct error *err)
{
	if (!frame_iscall(s->f)) {
		assert(s->prev);
		return stack_trace(s->prev, err);
	}
	char *ctx = error_context(s);
	char *loc = program_loc(frame_program(s->f));
	struct error *e = error_printf(
		"%s: %w (%s)%s", loc, err, stack_propername(s), ctx
	);
	free(loc);
	free(ctx);
	return e;
}

static char *
stack_context(struct stack *);

char *
error_context(struct stack *s)
{
	struct strbuilder *b = strbuilder_create();
	if (s->prev) {
		char *context = stack_context(s->prev);
		if (strlen(context)) {
			strbuilder_printf(b, "%s", context);
		}
		free(context);
	}
	return strbuilder_build(b);
}

static char *
stack_context(struct stack *s)
{
	struct strbuilder *b = strbuilder_create();
	if (frame_iscall(s->f)) {
		char *loc = program_loc(frame_program(s->f));
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

static char *
stack_propername(struct stack *s)
{
	return frame_iscall(s->f) ? frame_name(s->f) : stack_propername(s->prev);
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
	if (!v && !frame_iscall(s->f)) {
		/* ⊢ block */
		return stack_getvariable(s->prev, id);
	}
	return v;
}

void
stack_popprep(struct stack *s, struct state *state)
{
	if (frame_iscall(s->f) && !ast_function_isvoid(frame_function(s->f))) {
		assert(state_readregister(state));
	}
}

struct block *
stack_getblock(struct stack *s, int address)
{
	assert(address < block_arr_nblocks(s->memory));

	return block_arr_blocks(s->memory)[address];
}

struct frame {
	char *name;
	enum frame_kind {
		FRAME_BLOCK,
		FRAME_LINEAR,
		FRAME_CALL,
		FRAME_SETUP,
	} kind;
	struct program *p;

	struct ast_expr *call;
	struct ast_function *f;
};

static struct frame *
frame_create(char *n, struct program *p, enum frame_kind kind)
{
	assert(p);

	struct frame *f = malloc(sizeof(struct frame));
	f->name = dynamic_str(n);
	f->p = p;
	f->kind = kind;
	f->call = NULL; /* for call type frame only */
	f->f = NULL;	/* for call type frame only */
	return f;
}

static struct frame *
frame_call_create(char *n, struct program *p, struct ast_expr *call,
		struct ast_function *func)
{
	assert(call); assert(func);

	struct frame *f = frame_create(n, p, FRAME_CALL);
	f->call = ast_expr_copy(call);
	f->f = ast_function_copy(func);
	return f;
}

struct frame *
frame_callabstract_create(char *n, struct ast_block *b, struct ast_expr *call,
		struct ast_function *func)
{
	return frame_call_create(n, program_abstract_create(b), call, func);
}

struct frame *
frame_callactual_create(char *n, struct ast_block *b, struct ast_expr *call,
		struct ast_function *func)
{
	return frame_call_create(n, program_actual_create(b), call, func);
}

static struct frame *
frame_block_create_withprogram(char *n, struct program *p)
{
	return frame_create(n, p, FRAME_BLOCK);
}

struct frame *
frame_blockverify_create(char *name, struct ast_block *b)
{
	return frame_block_create_withprogram(name, program_verify_create(b));
}

struct frame *
frame_blockfindsetup_create(char *name, struct ast_block *b)
{
	return frame_block_create_withprogram(name, program_findsetup_create(b));
}

struct frame *
frame_blocksame_create(char *name, struct ast_block *b, struct state *s)
{
	return frame_block_create_withprogram(
		name, program_same_create(b, state_stack(s)->f->p)
	);
}

struct frame *
frame_setup_create(char *n, struct ast_block *b)
{
	return frame_create(n, program_setup_create(b), FRAME_SETUP);
}

struct frame *
frame_linear_create(char *n, struct ast_block *b, struct state *s)
{
	return frame_create(
		n,
		program_same_create(b, state_stack(s)->f->p),
		FRAME_LINEAR
	);
}

static void
frame_destroy(struct frame *f)
{
	free(f->name);
	program_destroy(f->p);
	if (frame_iscall(f)) {
		ast_expr_destroy(f->call);
		ast_function_destroy(f->f);
	}
}

static struct frame *
frame_copy(struct frame *f)
{
	struct frame *copy = frame_create(f->name, program_copy(f->p), f->kind);
	if (f->kind == FRAME_CALL) {
		copy->call = ast_expr_copy(f->call);
		copy->f = ast_function_copy(f->f);
	}
	return copy;
}

static void
frame_setname(struct frame *f, char *new)
{
	free(f->name);
	f->name = new;
}

static char *
frame_name(struct frame *f)
{
	return f->name;
}

static int
frame_modecanverify(struct frame *f)
{
	return program_modecanverify(f->p);
}

static int
frame_modecanrunxr0cmd(struct frame *f)
{
	return program_modecanrunxr0cmd(f->p);
}

static struct program *
frame_program(struct frame *f)
{
	return f->p;
}

static struct ast_function *
frame_function(struct frame *f)
{
	assert(frame_iscall(f));
	return f->f;
}

static int
frame_iscall(struct frame *f)
{
	return f->kind == FRAME_CALL;
}

static int
frame_isblock(struct frame *f)
{
	return f->kind == FRAME_BLOCK;
}

static int
frame_isinter(struct frame *f)
{
	return f->kind == FRAME_LINEAR;
}

static int
frame_issetup(struct frame *f)
{
	return f->kind == FRAME_SETUP;
}

struct ast_expr *
frame_call(struct frame *f)
{
	switch (f->kind) {
	case FRAME_CALL:
		return f->call;
	case FRAME_LINEAR:
		return program_prevcall(f->p);
	default:
		assert(false);
	}
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
	assert(
		block_res_as_block(
			location_getblock(v->loc, NULL, NULL, stack, NULL, NULL)
		)
	);

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
