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
	.alloc result;
]{
	return malloc(1);
}

void *
test3(int x) [
	assume: x;
	assume: x;
	.alloc result;
]{
	return malloc(1);
}

void *
test4(int x) [
	assume: x;
	assume: !x;
	/* should ignore everything after the contradiction */
	.alloc result;
]{
	return NULL;
}
