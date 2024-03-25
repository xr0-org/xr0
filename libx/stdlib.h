#ifndef STDLIB_H
#define STDLIB_H

#include <stddef.h>

axiom void *
malloc(int size) ~ [ return .malloc(size); ];

axiom void
free(void *ptr) ~ [ .free(ptr); ];

axiom void
exit(int status);

#endif
