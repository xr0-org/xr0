#ifdef TYPE

#ifndef TYPED
#define TYPED(THING) THING
#endif

struct TYPED(map_entry) {
	char *key;
	const TYPE value;
};

struct TYPED(map) {
	struct TYPED(map_entry) *entry;
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
TYPED(map_destroy) (struct TYPED(map) *map);

/**
 * Get index
 */
static int
TYPED(map_getindex) (struct TYPED(map) *map, const char *key);

/**
 * Get value
 */
TYPE
TYPED(map_get) (struct TYPED(map) *map, const char *key);

/**
 * Set value
 */
void
TYPED(map_set) (struct TYPED(map) *map, const char *key, TYPE val);

#endif
