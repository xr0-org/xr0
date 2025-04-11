#include <stdlib.h>
#include <assert.h>

#include "util.h"

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
