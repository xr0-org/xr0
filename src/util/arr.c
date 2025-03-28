#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "util.h"

struct string_arr {
	int n;
	char **s;
};

struct string_arr *
string_arr_create(void)
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
int_arr_create(void)
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
