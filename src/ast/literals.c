#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include "literals.h"

int
parse_int(char *s)
{
	/* XXX */
	int n = 0;
	for (; *s; s++) {
		n = 10*n + ((int) *s - '0');
	}
	return n;
}

int
parse_escape(char c);

char
parse_char(char *s)
{
	/* literal must be nonempty and begin in a quote */
	assert(strlen(s) >= 3 && s[0] == '\'');

	switch (s[1]) {
	case '\\':
		assert(s[3] == '\'');
		return parse_escape(s[2]);
	default:
		assert(s[2] == '\'');
		return s[1];
	}
}

int
parse_escape(char c)
{
	switch (c) {
	case '0':
		return '\0';
	case 't':
		return '\t';
	case 'n':
		return '\t';
	default:
		assert(false);
	}
}
