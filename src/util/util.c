#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include "util.h"
#include "ast.h"
#include "ext.h"

char *
dynamic_str(const char *s)
{
	int len = strlen(s) + 1;
	char *t = malloc(sizeof(char) * len);
	strncpy(t, s, len);
	return t;
}

char *
indentation(int level)
{
	assert(level >= 0);
	char *s = malloc(sizeof(char) * level + 1);
	for (int i = 0; i < level; i++) {
		s[i] = INDENT_CHAR;
	}
	s[level] = '\0';
	return s;
}


static struct entry
entry_create(const char *key, const void *value)
{
	assert(key != NULL);
	return (struct entry) { (char *) key, value };
}

static void
entry_destroy(struct entry e)
{
	free(e.key);
}

struct map *
map_create()
{
	return (struct map *) calloc(1, sizeof(struct map));
}

void
map_destroy(struct map *map)
{
	for (int i = 0; i < map->n; i++) {
		entry_destroy(map->entry[i]);
	}
	free(map->entry);
	free(map);
}

static int
map_getindex(struct map *map, const char *key)
{
	assert(key != NULL);
	for (int i = 0; i < map->n; i++) {
		if (strcmp(map->entry[i].key, key) == 0) {
			return i;
		}
	}
	return -1;
}

void *
map_get(struct map *map, const char *key)
{
	int index = map_getindex(map, key);
	if (index != -1) {
		return (void *) map->entry[index].value;
	}
	return NULL;
}

void
map_set(struct map *map, const char *key, const void *value)
{
	int index = map_getindex(map, key);
	if (index >= 0) {
		map->entry[index].value = value;
		return;
	} 
	map->entry = realloc(map->entry, sizeof(struct entry) * ++map->n);
	map->entry[map->n-1] = entry_create(key, value);
	return;
}


struct strbuilder {
	size_t cap;
	char *buf;
};

#define CAP_DEFAULT 100
#define CAP_MULT 2

struct strbuilder *
strbuilder_create()
{
	struct strbuilder *b = malloc(sizeof(struct strbuilder));
	b->cap = CAP_DEFAULT;
	b->buf = malloc(sizeof(char) * b->cap);
	b->buf[0] = '\0';
	return b;
}

char *
strbuilder_build(struct strbuilder *b)
{
	assert(b != NULL);
	int len = strlen(b->buf) + 1;
	char *s = malloc(sizeof(char) * len);
	snprintf(s, len, "%s", b->buf);
	free(b->buf);
	free(b);
	return s;
}

char *
strbuilder_preview(struct strbuilder *b)
{
	assert(b != NULL);
	int len = strlen(b->buf) + 1;
	char *s = malloc(sizeof(char) * len);
	snprintf(s, len, "%s", b->buf);
	return s;
}

static void
strbuilder_realloc(struct strbuilder *b, size_t len)
{
	/* cap must be strictly > for null termination */
	while (b->cap <= len) {
		b->cap *= CAP_MULT;
		b->buf = realloc(b->buf, sizeof(char) * b->cap);
	}
}

static void
strbuilder_append(struct strbuilder *b, char *s, size_t len)
{
	int buflen = strlen(b->buf);
	strbuilder_realloc(b, buflen + len);
	size_t newlen = buflen + len + 1;
	strncpy(b->buf + buflen, s, newlen - buflen);
}

int
strbuilder_vprintf(struct strbuilder *b, const char *fmt, va_list ap)
{
	size_t len; char *buf;
	FILE *out = open_memstream(&buf, &len);
	int r = vfprintf(out, fmt, ap);
	fclose(out);
	strbuilder_append(b, buf, len);
	free(buf);
	return r;
}

int
strbuilder_printf(struct strbuilder *b, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	int r = strbuilder_vprintf(b, fmt, ap);
	va_end(ap);
	return r;
}

int
strbuilder_puts(struct strbuilder *b, char *s)
{
	size_t len = strlen(s);
	strbuilder_append(b, s, len);
	return len;
}

void
strbuilder_putc(struct strbuilder *b, char c)
{
	strbuilder_printf(b, "%c", c);
}

struct string_arr {
	int n;
	char **s;
};

struct string_arr *
string_arr_create()
{
	struct string_arr *arr = calloc(1, sizeof(struct string_arr));
	assert(arr);
	return arr;
}

void
string_arr_destroy(struct string_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		free(arr->s[i]);
	}
	free(arr->s);
	free(arr);
}

char **
string_arr_s(struct string_arr *arr)
{
	return arr->s;
}

int
string_arr_n(struct string_arr *arr)
{
	return arr->n;
}

/* string_arr_append: Append struct node to array and return index (address). */
int
string_arr_append(struct string_arr *arr, char *s)
{
	arr->s = realloc(arr->s, sizeof(char *) * ++arr->n);
	assert(arr->s);
	int loc = arr->n-1;
	arr->s[loc] = s;
	return loc;
}

struct string_arr *
string_arr_copy(struct string_arr *old)
{
	struct string_arr *new = string_arr_create();
	for (int i = 0; i < old->n; i++) {
		string_arr_append(new, dynamic_str(old->s[i]));
	}
	return new;
}

struct string_arr *
string_arr_concat(struct string_arr *s1, struct string_arr *s2)
{
	assert(s1 && s2);
	struct string_arr *new = string_arr_copy(s1);		
	for (int i = 0; i < s2->n; i++) {
		string_arr_append(new, s2->s[i]);
	}
	return new;
}

char *
string_arr_deque(struct string_arr *arr)
{
	char *ret = dynamic_str(arr->s[0]);
	for (int i = 0; i < arr->n-1; i++) {
		arr->s[i] = arr->s[i+1];
	}
	arr->s = realloc(arr->s, sizeof(char *) * --arr->n);
	return ret;
}

bool
string_arr_contains(struct string_arr *arr, char *s)
{
	for (int i = 0; i < arr->n; i++) {
		if (strcmp(s, arr->s[i]) == 0) {
			return true;
		}
	}
	return false;
}

char *
string_arr_str(struct string_arr *string_arr)
{
	struct strbuilder *b = strbuilder_create();
	char **s = string_arr->s;
	int n = string_arr->n;
	for (int i = 0; i < n; i++) {
		char *str = s[i];
		strbuilder_printf(b, "%s%s", str, (i + 1 < n) ? ", " : "");
	}
	return strbuilder_build(b);
}


struct int_arr {
	int len;
	int *arr;
};

struct int_arr *
int_arr_create()
{
	struct int_arr *arr = calloc(1, sizeof(struct int_arr));
	assert(arr);
	return arr;
}

void
int_arr_destroy(struct int_arr *arr)
{
	free(arr->arr);
	free(arr);
}

int *
int_arr_arr(struct int_arr *arr)
{
	return arr->arr;
}

int
int_arr_len(struct int_arr *arr)
{
	return arr->len;
}

void
int_arr_append(struct int_arr *arr, int num)
{
	arr->arr = realloc(arr->arr, sizeof(int *) * ++arr->len);
	assert(arr->arr);
	int loc = arr->len-1;
	arr->arr[loc] = num;
}

void
int_arr_appendrange(struct int_arr *arr, struct int_arr *arr2)
{
	int len = int_arr_len(arr2);
	int *arr2_arr = int_arr_arr(arr2);
	for (int i = 0; i < len; i++) {
		int_arr_append(arr, arr2_arr[i]);
	}
}


enum loglevel LOG_LEVEL;

/* d_printf: Print if Xr0 is in debug mode. */
int
d_printf(char *fmt, ...)
{
	if (LOG_LEVEL != LOG_DEBUG) {
		return 0;
	}
	va_list ap;
	va_start(ap, fmt);
	int r = vfprintf(stderr, fmt, ap);
	va_end(ap);
	return r;
}

/* v_printf: Print if Xr0 is in verbose mode. */
int
v_printf(char *fmt, ...)
{
	if (LOG_LEVEL != LOG_INFO) {
		return 0;
	}
	va_list ap;
	va_start(ap, fmt);
	int r = vfprintf(stderr, fmt, ap);
	va_end(ap);
	return r;
}

struct error {
	enum error_type {
		ERROR_PRINTF,
		ERROR_UNDECIDEABLE_COND,
		ERROR_RETURN,

		ERROR_BLOCK_OBSERVE_NOOBJ,

		ERROR_STATE_GET_NOBLOCK,
		ERROR_STATE_DEREF_RCONST,

		ERROR_MODULATE_SKIP,

		ERROR_EVAL_VOID,
	} type;
	union error_contents {
		char *printf;
		struct ast_expr *undecidable_cond;
	} contents;
	struct error *inner;
};

static struct error *
error_to(struct error *err, enum error_type t)
{
	assert(err);
	if (err->type == t) {
		return err;
	}
	if (err->inner) {
		return error_to(err->inner, t);
	}
	return NULL;
}

static char *
findnextfmt(char **start);

static struct error *
error_nest(struct error *outer, struct error *inner)
{
	if (!outer->inner) {
		outer->inner = inner;
		return outer;
	}
	return error_nest(outer->inner, inner);
}

struct error *
error_printf(char *fmt, ...)
{
	char *otherfmt; void *otherarg;
	struct error *inner;

	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_PRINTF;

	struct strbuilder *b = strbuilder_create();
	va_list ap;
	va_start(ap, fmt);
	for (char *p = fmt; *p; p++) {
		if (*p != '%') {
			strbuilder_putc(b, *p);
			continue;
		}
		/* ⊢ *p == '%' */
		switch (*++p) {
		case 'w':
			inner = va_arg(ap, struct error *);
			strbuilder_printf(
				b, "%s", error_str(inner)
			);
			err = error_nest(err, inner);
			break;
		default:
			otherfmt = findnextfmt(&p);
			otherarg = va_arg(ap, void *);
			strbuilder_printf(b, otherfmt, otherarg);
			free(otherfmt);
			p--; /* prepare for increment */
			break;
		}
	}
	char *s = strbuilder_build(b);
	va_end(ap);

	err->contents.printf = s;
	return err;
}

static char *
findnextfmt(char **p)
{
	char *start = *p;

	char *s = start;
	for (; *s && *s != '%'; s++) {}
	/* s is '\0' or '%' */
	int len = s - start;
	*p = s;
	char *output = malloc(sizeof(char) * (len + 2));
	*output = '%';
	strncpy(output + 1, start, len);
	*(output + len + 1) = '\0';
	return output;
}

struct error *
error_undecideable_cond(struct ast_expr *cond)
{
	assert(cond);

	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_UNDECIDEABLE_COND;
	err->contents.undecidable_cond = cond;
	return err;
}

struct error *
error_to_undecideable_cond(struct error *err)
{
	return error_to(err, ERROR_UNDECIDEABLE_COND);
}

struct ast_expr *
error_get_undecideable_cond(struct error *err)
{
	assert(err->type == ERROR_UNDECIDEABLE_COND);
	return err->contents.undecidable_cond;
}

struct error *
error_return()
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_RETURN;
	return err;
}

struct error *
error_to_return(struct error *err)
{
	return error_to(err, ERROR_RETURN);
}

struct error *
error_block_observe_noobj()
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_BLOCK_OBSERVE_NOOBJ;
	return err;
}

struct error *
error_to_block_observe_noobj(struct error *err)
{
	return error_to(err, ERROR_BLOCK_OBSERVE_NOOBJ);
}

struct error *
error_state_get_no_block()
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_STATE_GET_NOBLOCK;
	return err;
}

struct error *
error_to_state_get_no_block(struct error *err)
{
	return error_to(err, ERROR_STATE_GET_NOBLOCK);
}

struct error *
error_state_deref_rconst()
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_STATE_DEREF_RCONST;
	return err;
}

struct error *
error_to_state_deref_rconst(struct error *err)
{
	return error_to(err, ERROR_STATE_DEREF_RCONST);
}

struct error *
error_modulate_skip()
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_MODULATE_SKIP;
	return err;
}

struct error *
error_to_modulate_skip(struct error *err)
{
	return error_to(err, ERROR_MODULATE_SKIP);
}

struct error *
error_eval_void()
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_EVAL_VOID;
	return err;
}

struct error *
error_to_eval_void(struct error *err)
{
	return error_to(err, ERROR_EVAL_VOID);
}

char *
error_str(struct error *err)
{
	char *error_type_str[] = {
		[ERROR_UNDECIDEABLE_COND]	= "undecideable condition",
		[ERROR_RETURN]			= "returned",

		[ERROR_BLOCK_OBSERVE_NOOBJ]	= "block_observe no object",

		[ERROR_STATE_GET_NOBLOCK]	= "state_get no block",
		[ERROR_STATE_DEREF_RCONST]	= "state_deref rconst",

		[ERROR_MODULATE_SKIP]		= "modulate skip",

		[ERROR_EVAL_VOID]		= "eval void",
	};

	switch (err->type) {
	case ERROR_PRINTF:
		return dynamic_str(err->contents.printf);
	case ERROR_UNDECIDEABLE_COND:
	case ERROR_RETURN:
	case ERROR_BLOCK_OBSERVE_NOOBJ:
	case ERROR_STATE_GET_NOBLOCK:
	case ERROR_STATE_DEREF_RCONST:
	case ERROR_MODULATE_SKIP:
	case ERROR_EVAL_VOID:
		return dynamic_str(error_type_str[err->type]);
	default:
		assert(false);
	}
}

struct circuitbreaker {
	int n;
	void **obj;
};

struct circuitbreaker *
circuitbreaker_create()
{
	return calloc(1, sizeof(struct circuitbreaker));
}

struct circuitbreaker *
circuitbreaker_copy(struct circuitbreaker *old)
{
	struct circuitbreaker *new = malloc(sizeof(struct circuitbreaker));
	new->n = old->n;
	new->obj = malloc(sizeof(void *) * old->n);
	for (int i = 0; i < old->n; i++) {
		new->obj[i] = old->obj[i];
	}
	return new;
}

void
circuitbreaker_destroy(struct circuitbreaker *cb)
{
	free(cb);
}

bool
circuitbreaker_append(struct circuitbreaker *cb, void *obj)
{
	for (int i = 0; i < cb->n; i++) {
		if (cb->obj[i] == obj) {
			return false;
		}
	}
	cb->obj = realloc(cb->obj, sizeof(void *) * ++cb->n);
	assert(cb->obj);
	cb->obj[cb->n-1] = obj;
	return true;
}
