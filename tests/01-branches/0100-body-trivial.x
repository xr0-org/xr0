#include <stdlib.h>

void *
test(int x) ~ [ return .malloc(1); ]
{
	if (x) {
		return malloc(1);
	}
	return malloc(1);
}
