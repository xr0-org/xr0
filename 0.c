#include <stdio.h>

int
main()
{
	int x = 0;
	int y = 0;
	int z = 0;
	printf("x: %d, y: %d, z: %d\n", x, y, z);

}

struct list { int x; struct list * next; };

void
foo()
{
	struct  { char c; char d; } cd = { 0, 0 };
	struct point { int x; int y; int z; } p = (struct point) { 0, 0, 0 };
	struct point p2 = (struct point) { 0, 0, 0 };
	struct man { char * name; int age; } man = (struct man) { NULL, 0 };
	struct man john = (struct man) { NULL, 0 };
	struct list l1 = (struct list) { 0, NULL };
	int i = 0;
	abc: if (1) {
		struct point p3 = (struct point) { 0, 0, 0 };
		struct man james = (struct man) { NULL, 0 };
	} else {
		struct list l2 = (struct list) { 0, NULL };
	};

	for (i = 0; i<20; i++) {
		struct point p4 = (struct point) { 0, 0, 0 };
		struct man roger = (struct man) { NULL, 0 };
	}
}

