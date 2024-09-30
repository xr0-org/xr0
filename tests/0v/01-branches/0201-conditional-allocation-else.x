#include <stdlib.h>

#ifdef XR0

void *
test(int x) ~ [
	if (x) {
		return .malloc(1);
	} else {
		return .malloc(1);
	}
];

#endif

void *
test(int x)
{
	return malloc(1);
}
