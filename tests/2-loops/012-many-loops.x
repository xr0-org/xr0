#include <stdlib.h>

void
unit()
{
	int i;
	void **p;

	p = malloc(9);

	[ @p; ]

	for (i = 0; i < 9; i++) [ .alloc p[i]; ] {
		p[i] = malloc(1);
	}

	[ @p[4]; ]

	free(p[3]);

	free(p[6]);

	for (i = 0; i < 3; i++) [
		if (@p[i]) { .dealloc p[i]; } else { .undefined; }
	]{
		free(p[i]);
	}

	for (i = 5; i < 6; i++) [
		if (@p[i]) { .dealloc p[i]; } else { undefined; }
	]{
		free(p[i]);
	}

	for (i = 7; i < 9; i++) [
		if (@p[i]) { .dealloc p[i]; } else { undefined; }
	]{
		free(p[i]);
	}

	free(p[4]);

	free(p);
}
