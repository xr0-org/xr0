#include <stdlib.h>
#include <assert.h>

#include "util.h"

#include "arr.h"

struct arr {
	int n;
	void **p;
};

struct arr *
arr_create(void)
{
	struct arr *arr = calloc(1, sizeof(struct arr));
	assert(arr);
	return arr;
}

struct arr *
arr_copy(struct arr *old, void *copy(void *))
{
	int i;

	struct arr *new = arr_create();
	for (i = 0; i < old->n; i++)
		arr_append(new, copy(old->p[i]));
	return new;
}

struct arr *
arr_concat(struct arr *arr, struct arr *arr0)
{
	struct arr *new = arr_create();
	arr_appendall(new, arr);
	arr_appendall(new, arr0);
	return new;
}

void
arr_destroy(struct arr *arr, void destroy(void *))
{
	int i;

	for (i = 0; i < arr->n; i++)
		destroy(arr->p[i]);
	free(arr);
}

char *
arr_str(struct arr *arr, char *str(void *))
{
	int i;

	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "{");
	for (i = 0; i < arr->n; i++) {
		char *s = str(arr->p[i]);
		strbuilder_printf(b, "%s%s", s, i+1<arr->n ? ", " : "");
		free(s);
	}
	strbuilder_printf(b, "}");
	return strbuilder_build(b);
}

void
arr_append(struct arr *arr, void *p)
{
	arr->p = realloc(arr->p, sizeof(void *)*++arr->n);
	assert(arr->p);
	arr->p[arr->n-1] = p;
}

void
arr_appendall(struct arr *arr, struct arr *arr0)
{
	int i;

	for (i = 0; i < arr0->n; i++)
		arr_append(arr, arr0->p[i]);
}

int
arr_len(struct arr *arr) { return arr->n; }

void *
arr_get(struct arr *arr, int i) { return arr->p[i]; }
