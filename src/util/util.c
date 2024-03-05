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

struct error *
error_create(char *s)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->msg = s;
	return err;
}

struct error *
error_prepend(struct error* e, char *prefix)
{
	int new_len = strlen(prefix) + strlen(e->msg) + 1 + 1;
	char *new_msg = (char *) malloc(new_len);

	/* prepend prefix */
	strcpy(new_msg, prefix);
	/* concat original msg */
	strcat(new_msg, e->msg);
	/* add newline character */
	strcat(new_msg, "\n");

	free(e);

	return error_create(new_msg);
}

/* string_arr */

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
	arr->s = realloc(arr->s, sizeof(struct string_arr) * ++arr->n);
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


/* varinfo */

struct varinfo *
varinfo_create(struct ast_type *t, bool isderef, bool isrval)
{
	struct varinfo *v = malloc(sizeof(struct varinfo));
	v->type = t;
	v->isderef = isderef;
	v->isrval = isrval;
	return v;
}

void
varinfo_destroy(struct varinfo *v)
{
	/* varinfo just wraps a param the param should be destroyed separately */
	free(v);
}
