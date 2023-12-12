#ifndef XR0_UTIL_H
#define XR0_UTIL_H
#include <stddef.h>
#include <stdarg.h>
#include <stdbool.h>

#define LEN(a) (sizeof(a) / sizeof((a)[0]))

char *
dynamic_str(const char *);


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


struct error {
	char *msg;
	struct error *inner;
};

struct error *
error_create(char *s);

struct error *
error_prepend(struct error *, char *msg);

#endif
