/* 
 * thanks to jorendorff
 * https://github.com/xr0-org/xr0/issues/50
 */

#include <stdlib.h>

#ifdef XR0

void *
test(int x, int y) ~ [
	if (x) {
		return .malloc(1);
	} else if (y) {
		return NULL;
	} else {
		return .malloc(1);
	}
];

#endif

void *
test(int x, int y)
{
	if (!y) {
		return malloc(1);
	}
	return NULL;
}
