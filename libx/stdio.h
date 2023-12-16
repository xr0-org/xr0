#ifndef STDIO_H
#define STDIO_H

typedef int FILE;

axiom int
putc(int c);

axiom int
puts(char *s);

axiom FILE *
fopen(char *pathname, char *mode);

axiom int
fseek(FILE *stream, int offset, int whence); /* TODO: convert offset to long */

/* TODO: define properly */
#define SEEK_END 0
#define SEEK_SET 0

#endif
