#ifndef STDIO_H
#define STDIO_H
#include <stddef.h>

typedef int FILE;

axiom int
putc(int c);

axiom int
puts(char *s);

axiom FILE *
fopen(char *pathname, char *mode);

axiom int
fseek(FILE *stream, int offset, int whence); /* TODO: convert offset to long */

#define SEEK_END 0
#define SEEK_SET 0

axiom int
ftell(FILE *stream);

axiom size_t
fread(void *ptr, size_t size, size_t nmemb, FILE *stream);

axiom int
fclose(FILE *stream);

#endif
