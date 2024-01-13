#ifndef STRING_H
#define STRING_H
#include <stddef.h>

axiom char *
strcpy(char *dest, char *src);

axiom char *
strncpy(char *dest, char *src, size_t n);

axiom size_t
strlen(char *s);

axiom int
strcmp(char *s1, char *s2);

axiom int
strncmp(char *s1, char *s2, size_t n);

#endif
