#ifndef STDLIB_H
#define STDLIB_H

axiom void *
malloc(int size) [ .alloc result; ];

axiom void
free(void *ptr) [
	if (@ptr)
		.dealloc ptr;
	else /* calling free on a non-allocated pointer */
		.undefined;
];

#endif
