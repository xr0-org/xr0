#include <stdlib.h>

void *
test(int x) [
	assume: x;
	.alloc result;
]{
	return NULL;
}

void *
test2(int x) [
	assume: !x;
]{
	return NULL;
}
