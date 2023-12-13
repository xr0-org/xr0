#include "stdio.h"
#include "stdlib.h"
#include "assert.h"
#include "string.h"
#include "generic_map.h"

TYPED(map_entry)
TYPED(entry_create) (char *key, const void *val)
{
	assert(key);
	return TYPED(map_entry) { (char *) key, val };
}

void
TYPED(entry_destroy) (TYPED(map_entry) e) {
	free(e.key);
}

struct TYPED(map) *
TYPED(map_create) () {
	return (struct TYPED(map) *) calloc(1, sizeof(struct TYPED(map)));
}

void
TYPED(map_destroy) (struct TYPED(map) *m) {
	for (int i = 0; i < m->n; i++) {
		entry_destroy(m->entry[i]);
	}
}

static int
TYPED(map_getindex) (struct TYPED(map) *m, const char *key) {
	assert(key != NULL);
	for (int i = 0; i < m->n; i++) {
		if (strcmp(m->entry[i].key, key) == 0) {
			return i;
		}
	}
	return -1;
}

TYPE
TYPED(map_get) (struct TYPED(map) *m, const char *key)
{
	int index = map_getindex(m, key);
	if (index != -1) {
		return (void *) m->entry[index].val;
	}
	return NULL;
}

void
TYPED(map_set) (struct TYPED(map) *m, char *key, const void *value)
{
	int index = map_getindex(m, key);
	if (index >= 0) {
		m->entry[index].val = value;
		return;
	}
	m->entry = realloc(m->entry, sizeof(struct TYPED(map_entry)) * ++m->n);
	m->entry[m->n-1] = TYPED(entry_create) (key, value);
	return;
}
