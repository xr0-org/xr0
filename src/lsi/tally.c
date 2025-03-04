#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "util.h"

#include "tally.h"


/* _tally_add: add u to t */
static void
_tally_add(struct tally *t, struct tally *u);

struct tally *
tally_sum(struct tally *t, struct tally *u)
{
	struct tally *sum = tally_create();
	_tally_add(sum, t);
	_tally_add(sum, u);
	return sum;
}

static void
_tally_add(struct tally *t, struct tally *u)
{
	int i;
	struct string_arr *arr = tally_getvars(u);

	for (i = 0; i < string_arr_n(arr); i++) {
		char *var = string_arr_s(arr)[i];
		tally_setcoef(
			t,
			dynamic_str(var),
			tally_getcoef(t, var) + tally_getcoef(u, var)
		);
	}
	tally_setconst(t, tally_getconst(t) + tally_getconst(u));

	string_arr_destroy(arr);
}

static void
_tally_multiply(struct tally *t, int n);

struct tally *
tally_product(struct tally *t, int n)
{
	struct tally *product = tally_copy(t);
	_tally_multiply(product, n);
	return product;
}

static void
_tally_multiply(struct tally *t, int n)
{
	int i;
	struct string_arr *arr = tally_getvars(t);

	for (i = 0; i < string_arr_n(arr); i++) {
		char *var = string_arr_s(arr)[i];
		tally_setcoef(t, dynamic_str(var), n*tally_getcoef(t, var));
	}
	tally_setconst(t, n*tally_getconst(t));

	string_arr_destroy(arr);
}


struct tally {
	struct map *m;
	int c;
};

struct tally *
tally_create(void)
{
	struct tally *t = malloc(sizeof(struct tally));
	assert(t);
	t->m = map_create();
	t->c = 0;
	return t;
}

struct tally *
tally_copy(struct tally *old)
{
	int i;

	struct tally *new = tally_create();
	struct map *m = old->m;
	for (i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		map_set(new->m, dynamic_str(e.key), e.value);
	}
	new->c = old->c;

	return new;
}

void
tally_destroy(struct tally *t)
{
	map_destroy(t->m);
	free(t);
}

char *
tally_str(struct tally *t)
{
	int i;

	struct strbuilder *b = strbuilder_create();

	strbuilder_putc(b, '{');
	struct map *m = t->m;
	for (i = 0; i < m->n; i++) {
		struct entry e = m->entry[i];
		strbuilder_printf(
			b, 
			"\'%s\': %d%s", 
			e.key, e.value,
			i+1 < m->n ? ", " : ""
		);
	}
	strbuilder_printf(b, "; %d}", t->c);

	return strbuilder_build(b);
}

int
tally_getcoef(struct tally *t, char *var)
{
	return (long) map_get(t->m, var);
}

int
tally_getconst(struct tally *t)
{
	return t->c;
}

void
tally_setcoef(struct tally *t, char *var, int c)
{
	map_set(t->m, var, (void *) (long) c);
}

void
tally_setconst(struct tally *t, int c)
{
	t->c = c;
}

struct string_arr;

struct string_arr *
tally_getvars(struct tally *t)
{
	int i;

	struct map *m = t->m;
	struct string_arr *arr = string_arr_create();
	for (i = 0; i < m->n; i++) {
		string_arr_append(arr, dynamic_str(m->entry[i].key));
	}
	return arr;
}
