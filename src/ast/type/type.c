#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "ext.h"
#include "state.h"
#include "type.h"
#include "util.h"
#include "value.h"

struct ast_type {
	int mod;
	enum ast_type_base base;
	union {
		struct ast_type *ptr_type;
		struct {
			struct ast_type *type;
			int length;
		} arr;
		struct {
			char *tag;
			struct ast_variable_arr *members;
		} structunion;
		char *userdef;
	};
};

bool
ast_type_isarr(struct ast_type *t)
{
	return t->base == TYPE_ARRAY;
}

bool
ast_type_isptr(struct ast_type *t)
{
	return t->base == TYPE_POINTER;
}

struct ast_type *
ast_type_arr_type(struct ast_type *t)
{
	assert(ast_type_isarr(t));
	return t->arr.type;
}

bool
ast_type_isvoid(struct ast_type *t)
{
	return t->base == TYPE_VOID;
}

struct ast_type *
ast_type_create(enum ast_type_base base, enum ast_type_modifier mod)
{
	struct ast_type *t = malloc(sizeof(struct ast_type));
	assert(t);
	t->base = base;
	t->mod = mod;
	return t;
}

struct ast_type *
ast_type_create_int()
{
	return ast_type_create(TYPE_INT, 0);
}

struct ast_type *
ast_type_create_ptr(struct ast_type *ref)
{
	assert(ref);
	struct ast_type *t = ast_type_create(TYPE_POINTER, 0);
	t->ptr_type = ref;
	return t;
}

struct ast_type *
ast_type_create_char()
{
	return ast_type_create(TYPE_CHAR, 0);
}

struct ast_type *
ast_type_create_void()
{
	return ast_type_create(TYPE_VOID, 0);
}

struct ast_type *
ast_type_create_voidptr()
{
	struct ast_type *t = ast_type_create(TYPE_POINTER, 0);
	t->ptr_type = ast_type_create(TYPE_VOID, 0);
	return t;
}

struct ast_type *
ast_type_create_arr(struct ast_type *base, int length)
{
	assert(base);
	struct ast_type *t = ast_type_create(TYPE_ARRAY, 0);
	t->arr.type = base;
	t->arr.length = length;
	return t;
}

struct ast_type *
ast_type_create_struct(char *tag, struct ast_variable_arr *members)
{
	struct ast_type *t = ast_type_create(TYPE_STRUCT, 0);
	t->structunion.tag = tag;
	t->structunion.members = members;
	return t;
}

struct ast_type *
ast_type_create_userdef(char *name)
{
	struct ast_type *t = ast_type_create(TYPE_USERDEF, 0);
	t->userdef = name;
	return t;
}

struct value *
ast_type_rconst(struct ast_type *t, struct state *s, struct ast_expr *range,
		char *comment, bool persist)
{
	switch (t->base) {
	case TYPE_INT:
		assert(range);
		return value_int_indefinite_create(range);
	case TYPE_POINTER:
		return value_ptr_indefinite_create(t->ptr_type);
	case TYPE_USERDEF:
		return ast_type_rconst(
			externals_gettypedef(state_getext(s), t->userdef), s,
			range, comment, persist
		);
	case TYPE_STRUCT:
		return value_struct_indefinite_create(t, s, comment, persist);
	case TYPE_RANGE:
		assert(range);
		return value_int_indefinite_create(range);
	default:
		assert(false);
	}
}

bool
ast_type_isstruct(struct ast_type *t)
{
	return t->base == TYPE_STRUCT;
}

struct ast_type *
ast_type_struct_complete(struct ast_type *t, struct externals *ext)
{
	if (ast_type_struct_members(t)) {
		return t;
	}
	char *tag = ast_type_struct_tag(t);
	assert(tag);
	return externals_getstruct(ext, tag);
}

struct ast_variable_arr *
ast_type_struct_members(struct ast_type *t)
{
	assert(t->base == TYPE_STRUCT);

	return t->structunion.members;
}

struct ast_type *
ast_type_struct_membertype(struct ast_type *t, char *field, struct externals *ext)
{
	struct ast_type *complete = ast_type_struct_complete(t, ext);
	struct ast_variable_arr *arr = complete->structunion.members;
	int n = ast_variable_arr_n(arr);
	struct ast_variable **v = ast_variable_arr_v(arr);
	for (int i = 0; i < n; i++) {
		if (strcmp(ast_variable_name(v[i]), field) == 0) {
			return ast_variable_type(v[i]);
		}
	}
	assert(false);
}

char *
ast_type_struct_tag(struct ast_type *t)
{
	assert(t->base == TYPE_STRUCT);

	return t->structunion.tag;
}

struct ast_type *
ast_type_create_struct_anonym(struct ast_variable_arr *members)
{
	return ast_type_create_struct(NULL, members);
}

struct ast_type *
ast_type_create_struct_partial(char *tag)
{
	return ast_type_create_struct(tag, NULL);
}

struct ast_type *
ast_type_copy_struct(struct ast_type *old)
{
	assert(old->base == TYPE_STRUCT);

	struct ast_type *new = ast_type_create(TYPE_STRUCT, old->mod);
	new->structunion.tag = old->structunion.tag
		? dynamic_str(old->structunion.tag)
		: NULL;
	new->structunion.members = old->structunion.members
		? ast_variable_arr_copy(old->structunion.members)
		: NULL;
	return new;
}

struct ast_type *
ast_type_create_range()
{
	return ast_type_create(TYPE_RANGE, 0);
}

void
ast_type_mod_or(struct ast_type *t, enum ast_type_modifier m)
{
	t->mod |= m;
}

bool
ast_type_istypedef(struct ast_type *t)
{
	return t->mod & MOD_TYPEDEF;
}

void
ast_type_destroy(struct ast_type *t)
{
	switch (t->base) {
	case TYPE_POINTER:
		assert(t->ptr_type);
		ast_type_destroy(t->ptr_type);
		break;
	case TYPE_ARRAY:
		assert(t->arr.type);
		ast_type_destroy(t->arr.type);
	default:
		break;
	}
	free(t);
}

struct ast_type *
ast_type_copy(struct ast_type *t)
{
	assert(t);
	switch (t->base) {
	case TYPE_POINTER:
		return ast_type_create_ptr(
			ast_type_copy(t->ptr_type)
		);
	case TYPE_ARRAY:
		return ast_type_create_arr(
			ast_type_copy(t->arr.type),
			t->arr.length
		);
	case TYPE_STRUCT:
		return ast_type_copy_struct(t);
	case TYPE_USERDEF:
		return ast_type_create_userdef(dynamic_str(t->userdef));

	case TYPE_VOID:
	case TYPE_INT:
	case TYPE_CHAR:
		return ast_type_create(t->base, t->mod);

	case TYPE_RANGE:
		return ast_type_create_range();

	default:
		assert(false);
	}
}

static void
ast_type_str_build_ptr(struct strbuilder *b, struct ast_type *t);

static void
ast_type_str_build_arr(struct strbuilder *b, struct ast_type *t);

static void
ast_type_str_build_struct(struct strbuilder *b, struct ast_type *t);

static char *
mod_str(int mod);

const char *type_basestr[] = {
	[TYPE_VOID]	= "void",
	[TYPE_CHAR]	= "char",
	[TYPE_SHORT]	= "short",
	[TYPE_INT]	= "int",
	[TYPE_LONG]	= "long",
	[TYPE_FLOAT]	= "float",
	[TYPE_DOUBLE]	= "double",
	[TYPE_SIGNED]	= "signed",
	[TYPE_UNSIGNED]	= "unsigned",
	[TYPE_RANGE]	= "range",
};

char *
ast_type_str(struct ast_type *t)
{
	assert(t);
	struct strbuilder *b = strbuilder_create();
	char *mod = mod_str(t->mod);
	strbuilder_printf(b, "%s", mod);
	free(mod);
	switch (t->base) {
	case TYPE_POINTER:
		ast_type_str_build_ptr(b, t);
		break;
	case TYPE_ARRAY:
		ast_type_str_build_arr(b, t);
		break;
	case TYPE_STRUCT:
		ast_type_str_build_struct(b, t);
		break;
	case TYPE_USERDEF:
		strbuilder_printf(b, "%s", t->userdef);
		break;
	default:
		strbuilder_printf(b, type_basestr[t->base]);
		break;
	}
	return strbuilder_build(b);
}

static char *
mod_str(int mod)
{
	/* XXX */
	const char *modstr[] = {
		[MOD_TYPEDEF]   = "typedef",
		[MOD_EXTERN]	= "extern",
		[MOD_AUTO]	= "auto",
		[MOD_STATIC]	= "static",
		[MOD_REGISTER]	= "register",

		[MOD_CONST]	= "const",
		[MOD_VOLATILE]	= "volatile",
	};
	const int modlen = 7;
	struct strbuilder *b = strbuilder_create();
	int nmods = 0;
	for (int i = 0; i < modlen; i++) {
		if (1 << i & mod) {
			nmods++;
		}
	}
	for (int i = 0; i < modlen; i++) {
		int m = 1 << i;
		if (m & mod) {
			char *space = nmods-- ? " " : "";
			strbuilder_printf(b, "%s%s", modstr[m], space);
		}
	}
	return strbuilder_build(b);
}

static void
ast_type_str_build_ptr(struct strbuilder *b, struct ast_type *t)
{
	char *base = ast_type_str(t->ptr_type);
	bool space = t->ptr_type->base != TYPE_POINTER;
	strbuilder_printf(b, "%s%s*", base, space ? " " : "");
	free(base);
}

static void
ast_type_str_build_arr(struct strbuilder *b, struct ast_type *t)
{
	char *base = ast_type_str(t->arr.type);
	strbuilder_printf(b, "%s[%d]", base, t->arr.length);
	free(base);
}

static void
ast_type_str_build_struct(struct strbuilder *b, struct ast_type *t)
{
	char *tag = t->structunion.tag;
	struct ast_variable_arr *members = t->structunion.members;

	assert(tag || members);

	strbuilder_printf(b, "struct ");

	if (tag) {
		strbuilder_printf(b, tag);
	}

	if (!members) {
		return;
	}

	strbuilder_printf(b, " { ");
	int n = ast_variable_arr_n(members);
	struct ast_variable **v = ast_variable_arr_v(members);
	for (int i = 0; i < n; i++) {
		char *s = ast_variable_str(v[i]);
		strbuilder_printf(b, "%s; ", s);
		free(s);
	}
	strbuilder_printf(b, "}");
}

char *
ast_type_strwithvar(struct ast_type *t, char *var)
{
	/* TODO: make print with normal C syntax */
	struct strbuilder *b = strbuilder_create();
	char *s = ast_type_str(t);
	strbuilder_printf(b, "%s %s", s, var);
	free(s);
	return strbuilder_build(b);
}

enum ast_type_base
ast_type_base(struct ast_type *t)
{
	return t->base;
}

struct ast_type *
ast_type_ptr_type(struct ast_type *t)
{
	assert(t->base == TYPE_POINTER);
	return t->ptr_type;
}

int
ast_type_size(struct ast_type *t)
{
	/* XXX: simplistic sizing until we study the alignment rules more
	 * closely */

	switch (t->base) {
	case TYPE_VOID:
	case TYPE_CHAR:
	case TYPE_INT:
	case TYPE_POINTER:
		return 1;

	case TYPE_ARRAY:
		return t->arr.length * ast_type_size(t->arr.type);

	case TYPE_STRUCT:
	case TYPE_USERDEF:
		return 1;
	default:
		assert(false);
	}
}

struct ast_type *
ast_type_deref(struct ast_type *t)
{
	switch (t->base) {
	case TYPE_POINTER:
		return t->ptr_type;
	case TYPE_ARRAY:
		return t->arr.type;
	default:
		assert(false);
	}
}
