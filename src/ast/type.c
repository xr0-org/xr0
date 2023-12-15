#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "util.h"

struct ast_type {
	enum ast_type_modifier mod;
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
	} u;
};

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
ast_type_create_ptr(struct ast_type *ref)
{
	assert(ref);
	struct ast_type *t = ast_type_create(TYPE_POINTER, 0);
	t->u.ptr_type = ref;
	return t;
}

struct ast_type *
ast_type_create_arr(struct ast_type *base, int length)
{
	assert(base);
	struct ast_type *t = ast_type_create(TYPE_ARRAY, 0);
	t->u.arr.type = base;
	t->u.arr.length = length;
	return t;
}

struct ast_type *
ast_type_create_struct(char *tag, struct ast_variable_arr *members)
{
	struct ast_type *t = ast_type_create(TYPE_STRUCT, 0);
	t->u.structunion.tag = tag;
	t->u.structunion.members = members;
	return t;
}

struct ast_variable_arr *
ast_type_struct_members(struct ast_type *t)
{
	assert(t->base == TYPE_STRUCT);

	return t->u.structunion.members;
}

char *
ast_type_struct_tag(struct ast_type *t)
{
	assert(t->base == TYPE_STRUCT);

	return t->u.structunion.tag;
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
	new->u.structunion.tag = old->u.structunion.tag
		? dynamic_str(old->u.structunion.tag)
		: NULL;
	new->u.structunion.members = old->u.structunion.members
		? ast_variable_arr_copy(old->u.structunion.members)
		: NULL;
	return new;
}

void
ast_type_mod_or(struct ast_type *t, enum ast_type_modifier m)
{
	t->mod |= m;
}

void
ast_type_destroy(struct ast_type *t)
{
	switch (t->base) {
	case TYPE_POINTER:
		assert(t->u.ptr_type);
		ast_type_destroy(t->u.ptr_type);
		break;
	case TYPE_ARRAY:
		assert(t->u.arr.type);
		ast_type_destroy(t->u.arr.type);
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
			ast_type_copy(t->u.ptr_type)
		);
	case TYPE_ARRAY:
		return ast_type_create_arr(
			ast_type_copy(t->u.arr.type),
			t->u.arr.length
		);
	case TYPE_STRUCT:
		return ast_type_copy_struct(t);

	case TYPE_VOID:
	case TYPE_INT:
	case TYPE_CHAR:
		return ast_type_create(t->base, t->mod);

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

char *
ast_type_str(struct ast_type *t)
{
	assert(t);
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
	const int modlen = 6;
	const char *basestr[] = {
		[TYPE_VOID]	= "void",
		[TYPE_CHAR]	= "char",
		[TYPE_SHORT]	= "short",
		[TYPE_INT]	= "int",
		[TYPE_LONG]	= "long",
		[TYPE_FLOAT]	= "float",
		[TYPE_DOUBLE]	= "double",
		[TYPE_SIGNED]	= "signed",
		[TYPE_UNSIGNED]	= "unsigned",
	};
	struct strbuilder *b = strbuilder_create();
	int nmods = 0;
	for (int i = 0; i < modlen; i++) {
		if (1 << i & t->mod) {
			nmods++;
		}
	}
	for (int i = 0; i < modlen; i++) {
		int mod = 1 << i;
		if (mod & t->mod) {
			char *space = --nmods ? " " : "";
			strbuilder_printf(b, "%s%s", modstr[mod], space);
		}
	}
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
	default:
		strbuilder_printf(b, basestr[t->base]);
		break;
	}
	return strbuilder_build(b);
}

static void
ast_type_str_build_ptr(struct strbuilder *b, struct ast_type *t)
{
	char *base = ast_type_str(t->u.ptr_type);
	bool space = t->u.ptr_type->base != TYPE_POINTER;
	strbuilder_printf(b, "%s%s*", base, space ? " " : "");
	free(base);
}

static void
ast_type_str_build_arr(struct strbuilder *b, struct ast_type *t)
{
	char *base = ast_type_str(t->u.arr.type);
	strbuilder_printf(b, "%s[%d]", base, t->u.arr.length);
	free(base);
}

static void
ast_type_str_build_struct(struct strbuilder *b, struct ast_type *t)
{
	char *tag = t->u.structunion.tag;
	struct ast_variable_arr *members = t->u.structunion.members;

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

enum ast_type_base
ast_type_base(struct ast_type *t)
{
	return t->base;
}

struct ast_type *
ast_type_ptr_type(struct ast_type *t)
{
	assert(t->base == TYPE_POINTER);
	return t->u.ptr_type;
}
