#include <stdlib.h>

int
f(int param);

int
g(int param);

void *
alloc_if(int num) ~ [
	if (num) {
		result = .alloc(1);
	}
];

void *
test(int x) ~ [
	if (f(g(x))) {
		result = .alloc(1);
	}
]{
	int m;
	int n;
	int k;

	m = x;
	~ [ m == x; ]
	n = g(m);
	~ [ n == g(x); ]
	return alloc_if(f(n));
}

int
f(int param)
{
	return param;
}

int
g(int param)
{
	return param;
}

void *
alloc_if(int num)
{
	if (num) {
		return malloc(1);
	}
	return NULL;
}
