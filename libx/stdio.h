#ifndef STDIO_H
#define STDIO_H

typedef int FILE;

axiom int
putc(int c);

axiom int
puts(char *s);

axiom FILE *
fopen(char *pathname, char *mode);

#endif
