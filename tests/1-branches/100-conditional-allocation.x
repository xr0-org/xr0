#include <stdlib.h>
#include <string.h>

void *
test(int x) [
	if (x) {
		.alloc result;
	}
]{
	if (x) {
		return malloc(1);
	}
	return NULL;
}

/*
INTERNALLY

void *
test(int x) [
	if (x) {
		if (x) {
			.alloc result;
		}
	}
]{
	return malloc(1);
}

void *
test(int x) [
	if (!x) {
		if (x) {
			.alloc result;
		}
	}
]{
	return NULL;
}

*/
