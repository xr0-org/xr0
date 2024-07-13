#include <stdlib.h>

foo(int num) ~ [
	if (num) {
		setup: num = 3;
	}
]{}

test(int x) ~ [
	if (x) {
	}
]{
	x = 3;
	foo(x);
}
