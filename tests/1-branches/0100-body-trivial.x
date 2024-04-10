#include <stdlib.h>

void *
test(int x) ~ [ return .malloc(1); ]
{
	if (x) {
		return malloc(2);
	}
	return malloc(1);
}
