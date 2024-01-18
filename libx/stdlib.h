#ifndef STDLIB_H
#define STDLIB_H
#include <stddef.h>

axiom void *
malloc(int size) ~ [ .alloc result; ];

axiom void
free(void *ptr) ~ [ .dealloc ptr; ];

/* XXX: how to represent macros? */
axiom void
exit(int);

#endif
