#ifndef XR0_UTIL_H
#define XR0_UTIL_H
#include <stddef.h>
#include <stdarg.h>
#include <stdbool.h>

#define LEN(a) (sizeof(a) / sizeof((a)[0]))

char *
dynamic_str(const char *);

#define INDENT_CHAR '\t'

char *
indentation(int level);


/* XXX: We know. */
struct map {
	struct entry {
		char *key;
		const void *value;
	} *entry;
	int n;
};

struct map *
map_create();

void
map_destroy(struct map *);

void *
map_get(struct map *, const char *key);

void
map_set(struct map *, const char *key, const void *value);


/* XXX: string_arr: to be macro */

struct string_arr {
	int n;
	char **s;
};

struct string_arr *
string_arr_create();

void
string_arr_destroy();

char **
string_arr_s(struct string_arr *);

int
string_arr_n(struct string_arr *);

int
string_arr_append(struct string_arr *, char *);

char *
string_arr_deque(struct string_arr *);

struct string_arr *
string_arr_concat(struct string_arr *s1, struct string_arr *s2);

bool
string_arr_contains(struct string_arr *, char *s);

char *
string_arr_str(struct string_arr *);


struct strbuilder;

struct strbuilder *
strbuilder_create();

int
strbuilder_printf(struct strbuilder *b, const char *fmt, ...);

int
strbuilder_vprintf(struct strbuilder *b, const char *fmt, va_list ap);

void
strbuilder_putc(struct strbuilder *b, char c);

int
strbuilder_puts(struct strbuilder *b, char *s);

char *
strbuilder_build(struct strbuilder *b);

char *
strbuilder_preview(struct strbuilder *b);

enum loglevel {
	LOG_NONE	= 1 << 0,
	LOG_INFO	= 1 << 1,
	LOG_DEBUG	= 1 << 2
};

int
d_printf(char *fmt, ...);

/* v_printf: Print if Xr0 is in verbose mode. */
int
v_printf(char *fmt, ...);

/* a_printf: assert and print as appropriate if there is an error. */
#define a_printf(expr, ...) if (!expr) { fprintf(stderr, __VA_ARGS__); assert(false); }

struct error;

struct error *
error_printf(char *fmt, ...);

struct ast_expr;

struct error *
error_undecideable_cond(struct ast_expr *cond);

struct error *
error_to_undecideable_cond(struct error *);

struct ast_expr *
error_get_undecideable_cond(struct error *);

struct error *
error_return();

struct error *
error_to_return(struct error *);

char *
error_str(struct error *);


struct circuitbreaker;

struct circuitbreaker *
circuitbreaker_create();

struct circuitbreaker *
circuitbreaker_copy(struct circuitbreaker *);

void
circuitbreaker_destroy(struct circuitbreaker *);

bool
circuitbreaker_append(struct circuitbreaker *, void *);

#endif
