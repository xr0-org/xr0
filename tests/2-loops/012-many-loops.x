#include <stdlib.h>

void
unit()
{
	int i;
	int j;
	void **p;

	p = malloc(9);

	~ [ @p; ]

	for (i = 0; i != 9; i++) ~ [
		for (j = 0; j != i; j++) { .alloc p[j]; }
		for (j = 0; j != 9; j++) { .alloc p[j]; }
	] {
		p[i] = malloc(1);
	}

	~ [ @p[4]; ]

	free(p[3]);

	free(p[6]);

	for (i = 0; i != 3; i++) ~ [
		for (j = 0; j != i; j++) { .dealloc p[j]; }
		for (j = 0; j != 3; j++) { .dealloc p[j]; }
	] {
		free(p[i]);
	}

	for (i = 5; i != 6; i++) ~ [
		for (j = 5; j != i; j++) { .dealloc p[j]; }
		for (j = 5; j != 6; j++) { .dealloc p[j]; }
	] {
		free(p[i]);
	}

	~ [ for (i = 7; i != 9; i++) { @p[i]; } ]

	for (i = 7; i != 9; i++) ~ [
		for (j = 7; j != i; j++) { .dealloc p[j]; }
		for (j = 7; j != 9; j++) { .dealloc p[j]; }
	] {
		free(p[i]);
	}


	free(p[4]);

	free(p);
}
