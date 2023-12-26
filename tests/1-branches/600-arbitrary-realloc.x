#include <stdlib.h>

axiom int
arbitrary(int x);

void **
test(int x) [
	int i;
	int n;

	
]{
	int i;
	int n;
	void *p;

	n = 0;
	for (i = x; arbitrary(i); i++) {
		n++;
	}
	return malloc(n);
}

bool
check_something(void *p);

bool
caller(int i) [

]{
	void *p;
	bool *res;

	p = test(i);
	res = check_something(ret);
	/* to free or not to free */
	free(res);

	return res;
}
