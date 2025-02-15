#ifndef STDLIB_H
#define STDLIB_H

#include <stddef.h>

axiom void *
malloc(int size) ~ [
	setup: size = [0?];
	if ([?])
		return .malloc(size);
	else
		return NULL;
];

axiom void
free(void *ptr) ~ [
	if (ptr) {
		setup: ptr = .malloc(1);
		.free(ptr);
	}
];

axiom void
exit(int status);

#endif
