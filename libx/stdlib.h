#ifndef STDLIB_H
#define STDLIB_H
#include <stddef.h>

axiom void *
malloc(int size) [ .alloc result; ];

axiom void
free(void *ptr) [ .dealloc ptr; ];

#endif
