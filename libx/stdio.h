#ifndef STDIO_H
#define STDIO_H

typedef struct FILE; 

FILE *stdout;

int
fputc(int c, FILE *stream);

int
putc(int c, FILE *stream);

int
fputs(char *s, FILE *stream);

int
puts(char *s);

#endif
