#ifndef STDLIB_H
#define STDLIB_H

#include <stddef.h>

axiom void *
malloc(int size) ~ [ return .alloc(size); ];

axiom void
free(void *ptr) ~ [ .dealloc(ptr); ];

axiom void
exit(int status);

#endif
