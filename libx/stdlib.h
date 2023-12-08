#ifndef STDLIB_H
#define STDLIB_H

axiom void *
malloc(int size) [ .alloc result; ];

axiom void
free(void *ptr) [ .dealloc ptr; ];

#endif
