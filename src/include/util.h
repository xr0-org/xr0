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
map_create(void);

void
map_destroy(struct map *);

void *
map_get(struct map *, const char *key);

void
map_set(struct map *, const char *key, const void *value);

/* DECLARE_ARRAY: declare an array of the given type, of the form
 * 	`struct NAME##_arr`.  */
#define DECLARE_ARRAY(TYPE, NAME) \
struct NAME##_arr; \
\
struct NAME##_arr *\
NAME##_arr_create(void); \
\
struct NAME##_arr * \
NAME##_arr_copy(struct NAME##_arr *); \
\
void \
NAME##_arr_destroy(struct NAME##_arr *); \
\
char * \
NAME##_arr_str(struct NAME##_arr *); \
\
void \
NAME##_arr_append(struct NAME##_arr *, TYPE); \
\
int \
NAME##_arr_len(struct NAME##_arr *); \
\
TYPE \
NAME##_arr_get(struct NAME##_arr *, int);

/* DEFINE_ARRAY: define an array declared with DEFINE_ARRAY. COPY, DESTORY
 * and STR are functions of the forms
 * 	TYPE (TYPE),
 * 	void (TYPE) and
 * 	char *(TYPE).
 */
#define DEFINE_ARRAY(TYPE, NAME, COPY, DESTROY, STR) \
struct NAME##_arr { struct arr *_; }; \
\
static struct NAME##_arr *\
_##NAME##_arr_create(struct arr *_) \
{ \
	struct NAME##_arr *arr = malloc(sizeof(struct NAME##_arr)); \
	assert(arr); \
	arr->_ = _; \
	return arr; \
} \
\
struct NAME##_arr *\
NAME##_arr_create(void) \
{ \
	return _##NAME##_arr_create(arr_create()); \
} \
\
static void * \
_NAME_copy(void *p); \
\
struct NAME##_arr * \
NAME##_arr_copy(struct NAME##_arr *arr) \
{ \
	return _##NAME##_arr_create(arr_copy(arr->_, _NAME_copy)); \
} \
\
static void * \
_NAME_copy(void *p) \
{ \
	return (void *) COPY((TYPE) p); \
} \
\
static void \
_NAME_destroy(void *p); \
\
void \
NAME##_arr_destroy(struct NAME##_arr *arr) \
{ \
	arr_destroy(arr->_, _NAME_destroy); \
	free(arr); \
} \
\
static void \
_NAME_destroy(void *p) \
{ \
	DESTROY((TYPE) p); \
} \
\
static char * \
_NAME_str(void *p); \
\
char * \
NAME##_arr_str(struct NAME##_arr *arr) \
{ \
	return arr_str(arr->_, _NAME_str); \
} \
\
static char * \
_NAME_str(void *p) \
{ \
	return STR((TYPE) p); \
} \
\
void \
NAME##_arr_append(struct NAME##_arr *arr, TYPE elem) \
{ \
	arr_append(arr->_, (void *) elem); \
} \
\
int \
NAME##_arr_len(struct NAME##_arr *arr) { return arr_len(arr->_); } \
\
TYPE \
NAME##_arr_get(struct NAME##_arr *arr, int i) \
{ \
	return (TYPE) arr_get(arr->_, i); \
}

DECLARE_ARRAY(char *, string)

struct int_arr;

struct int_arr *
int_arr_create(void);

void
int_arr_destroy(struct int_arr *);

int *
int_arr_arr(struct int_arr *);

int
int_arr_len(struct int_arr *);

void
int_arr_append(struct int_arr *, int);

void
int_arr_appendrange(struct int_arr *arr, struct int_arr *arr2);

struct strbuilder;

struct strbuilder *
strbuilder_create(void);

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
#define a_printf(expr, ...) if (!(expr)) { fprintf(stderr, __VA_ARGS__); assert(expr); }

/* macro for unimplemented functionality. */
#define TODO(msg) { fprintf(stderr, "%s not implemented.\n", msg); assert(0); }

struct error;

struct error *
error_printf(char *fmt, ...);

struct verifierinstruct;

struct error *
error_verifierinstruct(struct verifierinstruct *);

struct error *
error_to_verifierinstruct(struct error *);

struct verifierinstruct *
error_get_verifierinstruct(struct error *);

struct error *
error_verifiercontradiction(void);

struct error *
error_to_verifiercontradiction(struct error *);

struct error *
error_return(void);

struct error *
error_to_return(struct error *);

struct error *
error_break(void);

struct error *
error_to_break(struct error *);

struct error *
error_prev(void);

struct error *
error_to_prev(struct error *);

struct error *
error_cmdvalidation(void);

struct error *
error_to_cmdvalidation(struct error *);

struct error *
error_block_observe_noobj(void);

struct error *
error_to_block_observe_noobj(struct error *);

struct error *
error_state_get_no_block(void);

struct error *
error_to_state_get_no_block(struct error *err);

struct error *
error_state_deref_rconst(void);

struct error *
error_to_state_deref_rconst(struct error *err);

struct error *
error_value_bounds(struct error *inner);

struct error *
error_to_value_bounds(struct error *);

struct error *
error_modulate_skip(void);

struct error *
error_to_modulate_skip(struct error *);

struct error *
error_eval_void(void);

struct error *
error_to_eval_void(struct error *);

struct error *
error_lsi_notfeasible(void);

struct error *
error_to_lsi_notfeasible(struct error *);

char *
error_str(struct error *);


struct circuitbreaker;

struct circuitbreaker *
circuitbreaker_create(void);

struct circuitbreaker *
circuitbreaker_copy(struct circuitbreaker *);

void
circuitbreaker_destroy(struct circuitbreaker *);

bool
circuitbreaker_append(struct circuitbreaker *, void *);

#define DECLARE_RESULT_TYPE(TYPE, VNAME, RTNAME) \
struct RTNAME; \
\
struct RTNAME * \
RTNAME##_error_create(struct error *err); \
\
struct RTNAME * \
RTNAME##_empty_create(void); \
\
struct RTNAME * \
RTNAME##_##VNAME##_create(TYPE val); \
\
void \
RTNAME##_destroy(struct RTNAME *res); \
\
bool \
RTNAME##_iserror(struct RTNAME *res); \
\
struct error * \
RTNAME##_as_error(struct RTNAME *res); \
\
bool \
RTNAME##_has##VNAME(struct RTNAME *res); \
\
TYPE \
RTNAME##_as_##VNAME(struct RTNAME *res); \
\
void \
RTNAME##_errorignore(struct RTNAME *res);

#define DEFINE_RESULT_TYPE(TYPE, VNAME, DESTRUCT, RTNAME, CANBENULL) \
struct RTNAME { \
	/* if err is set it's an error.\
	 * if CANBENULL we look at hasvalue then val.
	 * otherwise we proceed to val directly. */ \
	struct error *err; \
	bool hasvalue; \
	TYPE val; \
}; \
\
struct RTNAME * \
RTNAME##_error_create(struct error *err) \
{ \
	assert(err); \
	struct RTNAME *r = malloc(sizeof(struct RTNAME)); \
	r->hasvalue = false; \
	r->val = (TYPE) 0; \
	r->err = err; \
	return r; \
} \
\
struct RTNAME * \
RTNAME##_empty_create(void) \
{ \
	struct RTNAME *r = malloc(sizeof(struct RTNAME)); \
	r->hasvalue = false; \
	r->val = (TYPE) 0; \
	r->err = NULL; \
	return r; \
} \
\
struct RTNAME * \
RTNAME##_##VNAME##_create(TYPE val) \
{ \
	a_printf(CANBENULL || val, "NULL value for struct %s\n", #RTNAME); \
	struct RTNAME *r = malloc(sizeof(struct RTNAME)); \
	r->hasvalue = true; \
	r->val = val; \
	r->err = NULL; \
	return r; \
} \
\
void \
RTNAME##_destroy(struct RTNAME *res) \
{ \
	assert(!res->err); \
	if (res->hasvalue) { DESTRUCT(res->val); } \
	free(res); \
} \
\
bool \
RTNAME##_iserror(struct RTNAME *res) \
{ \
	return res->err; \
} \
\
struct error * \
RTNAME##_as_error(struct RTNAME *res) \
{ \
	assert(RTNAME##_iserror(res)); \
	return res->err; \
} \
\
bool \
RTNAME##_has##VNAME(struct RTNAME *res) \
{ \
	assert(!RTNAME##_iserror(res)); \
	return res->hasvalue; \
} \
\
TYPE \
RTNAME##_as_##VNAME(struct RTNAME *res) \
{ \
	assert(RTNAME##_has##VNAME(res)); \
	return res->val; \
} \
\
void \
RTNAME##_errorignore(struct RTNAME *res) \
{ \
	assert(RTNAME##_iserror(res)); \
	res->err = NULL; \
}

#endif
