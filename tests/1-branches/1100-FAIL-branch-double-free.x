#include <stdlib.h>

void
foo(int a, int b)
{
       int *p;
       p = malloc(1);
       if (!a) {
		if (!b) {
			free(p);
		}
       }
       if (a) {
               free(p);
       }
       if (b) {
               free(p);
       }
}
