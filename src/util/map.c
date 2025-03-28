#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "util.h"

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
map_create(void)
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

