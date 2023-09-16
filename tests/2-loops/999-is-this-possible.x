#include <stdlib.h>

void *
cond_alloc(int cond) [
	if (cond) {
		:alloc result;
	}
]{
	if (cond) {
		return malloc(1);
	}
	return NULL;
}

void *
unit(int K) [ :alloc result; ]
{
	int i;
	void **p;
	void *q;

	if (K < 2 || K % 2) {
		return malloc(1);
	}

	p = malloc(sizeof(void *) * K);

	for (i = 0; i < K; i++) [
		if (i%2 == 0) {
			:alloc p[i];
		}
	]{
		p[i] = cond_alloc(i%2 == 0);
	}

	for (i = 0; i < K/2; i += 2) [ :free p[i]; ] {
		free(p[i]);
	}

	for (i = K/2; i < K-2; i++) [
		if (i%2 == 0) {
			:free p[i];
		}
	]{
		if (i%2 == 0) {
			free(p[i]);
		}
	}

	q = p[K-2];
	free(p);
	return q;
}
