#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>

#include "util.h"
#include "ast.h"
#include "ext.h"

char *
dynamic_str(const char *s)
{
	int len = strlen(s) + 1;
	char *t = malloc(sizeof(char) * len);
	strncpy(t, s, len);
	return t;
}

char *
indentation(int level)
{
	assert(level >= 0);
	char *s = malloc(sizeof(char) * level + 1);
	for (int i = 0; i < level; i++) {
		s[i] = INDENT_CHAR;
	}
	s[level] = '\0';
	return s;
}


enum loglevel LOG_LEVEL;

/* d_printf: Print if Xr0 is in debug mode. */
int
d_printf(char *fmt, ...)
{
	if (LOG_LEVEL != LOG_DEBUG) {
		return 0;
	}
	va_list ap;
	va_start(ap, fmt);
	int r = vfprintf(stderr, fmt, ap);
	va_end(ap);
	return r;
}

/* v_printf: Print if Xr0 is in verbose mode. */
int
v_printf(char *fmt, ...)
{
	if (LOG_LEVEL != LOG_INFO) {
		return 0;
	}
	va_list ap;
	va_start(ap, fmt);
	int r = vfprintf(stderr, fmt, ap);
	va_end(ap);
	return r;
}
