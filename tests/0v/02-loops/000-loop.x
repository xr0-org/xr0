#include <stdlib.h>

void
unit()
{
	int i;
	void *p;

	for (i = 0; i != 9; i++) {
		p = malloc(1);

		#ifdef XR0
		~ [ @p; ]
		#endif

		free(p);
	}
}
