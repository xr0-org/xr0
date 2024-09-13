#ifndef STDLIB_H
#define STDLIB_H

#include <stddef.h>

axiom void *
malloc(int size) ~ [
	setup: size = [0?];
	return .malloc(size);
];

axiom void
free(void *ptr) ~ [
	setup: ptr = malloc(1);
	.free(ptr);
];

axiom void
exit(int status);

#endif
