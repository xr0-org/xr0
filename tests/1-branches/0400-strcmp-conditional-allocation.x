#include <stdlib.h>
#include <string.h>

void *
test(char *s) ~ [
	assume: strcmp(s, "yes") == 0;
	if (strcmp(s, "yes") == 0) {
		.alloc result;
	}
]{
	return malloc(1);
}

void *
test2(char *s) ~ [
	assume: !(strcmp(s, "yes") == 0);
]{
	return NULL;
}
