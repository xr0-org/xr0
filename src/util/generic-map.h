#ifdef TYPE

#ifndef TYPED
#define TYPED(THING) THING
#endif

struct TYPED(map_entry) {
	char *key;
	const TYPE *val;
};

struct TYPED(map) {
	TYPED(entry) *entry;
	int n;
};

/**
 * Create a new map
 */
struct TYPED(map) *
TYPED(map_create) ();

/**
 * Free space allocated for map
 */
void
TYPED(map_destroy) (TYPED(map) *map);

/**
 * Get index
 */
static int
TYPED(map_getindex) (TYPED(map) *map, const char *key);

/**
 * Get value
 */
TYPE *
TYPED(map_get) (TYPED(map) *map, const char *key);

/**
 * Set value
 */
void
TYPED(map_set) (TYPED(map) *map, const char *key, TYPE *val);

#endif
