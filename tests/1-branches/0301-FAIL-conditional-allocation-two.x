/* 
 * thanks to jorendorff
 * https://github.com/xr0-org/xr0/issues/50
 */

#include <stdlib.h>

void *
test(int x, int y) ~ [
	if (x) {
		return .malloc(1);
	} else if (y) {
		return NULL;
	} else {
		return .malloc(1);
	}
]{
	if (!y) {
		return malloc(1);
	}
	return NULL;
}
