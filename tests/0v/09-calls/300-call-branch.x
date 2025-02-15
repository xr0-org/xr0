#include <stdlib.h>

void *
foo(int flag) ~ [ if (flag) return malloc(1); ]
{
	int k; int *p; void *q;

	p = &k;
	if (flag) {
		*p = 0;
	} else {
		*p = 1;
	}
	q = malloc(1);
	if (k) {
		free(q);
	}
	return q;
}

void
bar(int val)
{
	void *p;

	p = foo(val);
	if (val) {
		free(p);
	}
}

void
main()
{
	bar(1);
}
