#include <stdio.h>

int
main()
{
	int x;
	printf("x: %d\n", x);
}

struct list {
	int x;
	struct list *next;
};

void
foo()
{
	struct { char c; char d; } cd;

	struct point { int x; int y; int z; } p;
	struct point p2;

	struct man { char *name; int age; };
	struct man john;

	struct list l1;

	int i;

	if (1) {
		struct point p3;
		struct man james;
	}

	for (i = 0; i < 20; i++) {
		struct point p4;
		struct man roger;
	}
}
