#include <stdlib.h>

void **p;

void
unit(int i) [ .alloc p[i]; ]
{
	p[i] = malloc(1);
}
