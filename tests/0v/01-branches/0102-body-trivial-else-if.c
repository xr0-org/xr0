#include <stdlib.h>

void *
test(int x) ~ [ return .malloc(1); ];

void *
test(int x)
{
	if (x) {
		return malloc(1);
	} else if (x) {
		return malloc(1);
	} else {
		return malloc(1);
	}
}
