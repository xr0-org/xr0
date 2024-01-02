#include <stdlib.h>

void *
test(int x) [
	assume: x;
	.alloc result;
]{
	return malloc(1);
}

void *
test2(int x) [
	assume: !x;
]{
	return malloc(1);
}
